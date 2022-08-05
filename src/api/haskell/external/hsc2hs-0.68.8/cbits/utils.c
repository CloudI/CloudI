/* ----------------------------------------------------------------------------
   (c) The University of Glasgow 2006, Lifted from Bases

   Useful Win32 bits
   ------------------------------------------------------------------------- */

#if defined(_WIN32)

#include "HsBase.h"
#include <stdbool.h>
#include <stdint.h>
/* Using Secure APIs */
#define MINGW_HAS_SECURE_API 1
#include <wchar.h>
#include <windows.h>

/* Copied from getTempFileNameErrorNo in base's cbits/Win32Utils.c in GHC 8.10.
   Check there for any bugfixes first and please keep in sync when making
   changes.  */

bool __get_temp_file_name (wchar_t* pathName, wchar_t* prefix,
                           wchar_t* suffix, uint32_t uUnique,
                           wchar_t* tempFileName)
{
  int retry = 5;
  bool success = false;
  while (retry > 0 && !success)
    {
      // TODO: This needs to handle long file names.
      if (!GetTempFileNameW(pathName, prefix, uUnique, tempFileName))
        {
          maperrno();
          return false;
        }

      wchar_t* drive = malloc (sizeof(wchar_t) * _MAX_DRIVE);
      wchar_t* dir   = malloc (sizeof(wchar_t) * _MAX_DIR);
      wchar_t* fname = malloc (sizeof(wchar_t) * _MAX_FNAME);
      if (_wsplitpath_s (tempFileName, drive, _MAX_DRIVE, dir, _MAX_DIR,
                         fname, _MAX_FNAME, NULL, 0) != 0)
        {
          success = false;
          maperrno ();
        }
      else
        {
          wchar_t* temp = _wcsdup (tempFileName);
          if (wcsnlen(drive, _MAX_DRIVE) == 0)
            swprintf_s(tempFileName, MAX_PATH, L"%s\%s%s",
                      dir, fname, suffix);
          else
            swprintf_s(tempFileName, MAX_PATH, L"%s\%s\%s%s",
                      drive, dir, fname, suffix);
          success
             = MoveFileExW(temp, tempFileName, MOVEFILE_WRITE_THROUGH
                                               | MOVEFILE_COPY_ALLOWED) != 0;
          errno = 0;
          if (!success && (GetLastError () != ERROR_FILE_EXISTS || --retry < 0))
            {
              success = false;
              maperrno ();
              DeleteFileW (temp);
            }


          free(temp);
        }

      free(drive);
      free(dir);
      free(fname);
    }

  return success;
}
#endif