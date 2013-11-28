//
//  Copyright (C) 2009-2012 Artyom Beilis (Tonkikh)
//
//  Distributed under the Boost Software License, Version 1.0. (See
//  accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
#ifndef BOOSTER_BACKTRACE_H
#define BOOSTER_BACKTRACE_H

#include <booster/config.h>
#include <stdexcept>
#include <typeinfo>
#include <string>
#include <vector>
#include <iosfwd>

namespace booster {

    ///
    /// \brief Namespace that holds basic operations
    /// for implementing stack trace
    ///
    namespace stack_trace {
        ///
        /// \brief Record stack frame
        ///
        /// Records at most \a size stack frames returning the pointers to the running
        /// code into \a addresses vector that should have at least size places
        ///
        /// returns that number of actually recorded frames
        /// 
        BOOSTER_API int trace(void **addresses,int size);
        ///
        /// \brief Print stack trace
        ///
        /// Writes stack trace recorded \ref trace function of \a size size to the output stream
        ///
        BOOSTER_API void write_symbols(void *const *addresses,int size,std::ostream &);
        ///
        /// \brief Get stack trace information about a single address recorded
        ///
        BOOSTER_API std::string get_symbol(void *address);
        ///
        /// \brief Get stack trace information about multiple address recorded
        ///
        BOOSTER_API std::string get_symbols(void * const *address,int size);
    } // stack_trace

    ///
    /// \brief the class that records the stack trace when it is created,
    ///
    /// It is a base class for all exceptions that record stack trace
    ///

    class backtrace {
    public:
       
        ///
        /// The default number of recorded frames 
        ///
        static size_t const default_stack_size = 32;

        ///
        /// Create stack trace recording at most \a frames_no stack frames
        ///
        backtrace(size_t frames_no = default_stack_size) 
        {
            if(frames_no == 0)
                return;
            frames_.resize(frames_no,0);
            int size = stack_trace::trace(&frames_.front(),frames_no);
            frames_.resize(size);
        }

        virtual ~backtrace() throw()
        {
        }

        ///
        /// Get the actual number of recorded stack frames
        ///
        size_t stack_size() const
        {
            return frames_.size();
        }

        ///
        /// Get the returned address for the stack frame number \a frame_no 
        ///
        void *return_address(unsigned frame_no) const
        {
            if(frame_no < stack_size())
                return frames_[frame_no];
            return 0;
        }

        ///
        /// Print the stack trace frame for the frame \a frame_no to the stream \a out
        ///
        void trace_line(unsigned frame_no,std::ostream &out) const
        {
            if(frame_no < frames_.size())
                stack_trace::write_symbols(&frames_[frame_no],1,out);
        }

        ///
        /// Get a readable stack trace frame for the frame \a frame_no
        ///
        std::string trace_line(unsigned frame_no) const
        {
            if(frame_no < frames_.size())
                return stack_trace::get_symbol(frames_[frame_no]);
            return std::string();
        }

        ///
        /// Get full stack trace as a string
        ///
        std::string trace() const
        {
            if(frames_.empty())
                return std::string();
            return stack_trace::get_symbols(&frames_.front(),frames_.size());
        }

        ///
        /// Print full stack trace to a stream \a out
        ///
        void trace(std::ostream &out) const
        {
            if(frames_.empty())
                return;
            stack_trace::write_symbols(&frames_.front(),frames_.size(),out);
        }
    
    private:
        std::vector<void *> frames_;
    };

    ///
    /// \brief Same as std::exception but records stack trace
    ///
    class exception : public std::exception, public backtrace {
    public:
    };
    
    ///
    /// \brief Same as std::bad_cast but records stack trace
    ///
    class bad_cast : public std::bad_cast, public backtrace {
    public:
    };

    ///
    /// \brief Same as std::runtime_error but records stack trace
    ///
    class runtime_error: public std::runtime_error, public backtrace {
    public:
        explicit runtime_error(std::string const &s) : std::runtime_error(s) 
        {
        }
    };

    ///
    /// \brief Same as std::range_error but records stack trace
    ///
    class range_error: public std::range_error, public backtrace {
    public:
        explicit range_error(std::string const &s) : std::range_error(s) 
        {
        }
    };

    ///
    /// \brief Same as std::overflow_error but records stack trace
    ///
    class overflow_error: public std::overflow_error, public backtrace {
    public:
        explicit overflow_error(std::string const &s) : std::overflow_error(s) 
        {
        }
    };

    ///
    /// \brief Same as std::underflow_error but records stack trace
    ///
    class underflow_error: public std::underflow_error, public backtrace {
    public:
        explicit underflow_error(std::string const &s) : std::underflow_error(s) 
        {
        }
    };

    ///
    /// \brief Same as std::logic_error but records stack trace
    ///
    class logic_error: public std::logic_error, public backtrace {
    public:
        explicit logic_error(std::string const &s) : std::logic_error(s) 
        {
        }
    };

    ///
    /// \brief Same as std::domain_error but records stack trace
    ///
    class domain_error: public std::domain_error, public backtrace {
    public:
        explicit domain_error(std::string const &s) : std::domain_error(s) 
        {
        }
    };

    ///
    /// \brief Same as std::length_error but records stack trace
    ///
    class length_error: public std::length_error, public backtrace {
    public:
        explicit length_error(std::string const &s) : std::length_error(s) 
        {
        }
    };

    ///
    /// \brief Same as std::invalid_argument but records stack trace
    ///
    class invalid_argument : public std::invalid_argument, public backtrace {
    public:
        explicit invalid_argument(std::string const &s) : std::invalid_argument(s)
        {
        }
    };
    
    ///
    /// \brief Same as std::out_of_range but records stack trace
    ///
    class out_of_range : public std::out_of_range, public backtrace {
    public:
        explicit out_of_range(std::string const &s) : std::out_of_range(s)
        {
        }
    };

    /// \cond INTERNAL
    namespace details {
        class trace_manip {
        public:
            trace_manip(backtrace const *tr) :
                tr_(tr)
            {
            }
            std::ostream &write(std::ostream &out) const
            {
                if(tr_)
                    tr_->trace(out);
                return out;
            }
        private:
            backtrace const *tr_;
        };

        inline std::ostream &operator<<(std::ostream &out,details::trace_manip const &t)
        {
            return t.write(out);
        }
    }
    /// \endcond

    ///
    /// \brief manipulator that print stack trace for the exception \a e if it is derived from backtrace.
    ///
    /// For example:
    ///
    /// \code
    ///
    /// catch(std::exception const &e) {
    ///   std::cerr << e.what() << std::endl;
    ///   std::cerr << booster::trace(e);
    /// }
    ///
    /// \endcode
    ///
    template<typename E>
    details::trace_manip trace(E const &e)
    {
        backtrace const *tr = dynamic_cast<backtrace const *>(&e);
        return details::trace_manip(tr);
    }


} // booster

// from lib/backtrace/src/backtrace.cpp

//
//  Copyright (C) 2009-2012 Artyom Beilis (Tonkikh)
//
//  Distributed under the Boost Software License, Version 1.0. (See
//  accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
//#define BOOSTER_SOURCE

//#include <booster/backtrace.h>
#include "config.h" // small modifications to use autoconf checks

//#if (defined(__linux) && !defined(__UCLIBC__)) || defined(__APPLE__) || defined(__sun) || defined(__FreeBSD__)
//#  define BOOSTER_HAVE_DLADDR
//#endif

//#if defined(__GNUC__)
//#define BOOSTER_HAVE_ABI_CXA_DEMANGLE
//#endif

#ifdef BOOSTER_HAVE_EXECINFO
#include <execinfo.h>
#endif

#ifdef BOOSTER_HAVE_ABI_CXA_DEMANGLE
#include <cxxabi.h>
#endif

#ifdef BOOSTER_HAVE_DLADDR
#include <dlfcn.h>
#endif
#include <string.h>
#include <stdlib.h>
#include <ostream>
#include <sstream>
#include <iomanip>

#if defined(BOOSTER_WIN32)
#include <windows.h>
#include <psapi.h>
#endif

#if defined(BOOSTER_MSVC)
#include <stdlib.h>
#include <dbghelp.h>
#endif

#if defined(BOOSTER_HAVE_UNWIND_BACKTRACE)

extern "C" {
	extern void * _Unwind_GetIP (void *);
	extern int _Unwind_Backtrace(int (*)(void *,void *),void *);
}

#endif

namespace booster {

    namespace stack_trace {
        //
        // Linux already has backtrace based on _Unwind_Backtrace,
        // other uses frame pointers that do not work when -fomit-frame-pointer is used
        // so enable it on Apple, Sun and BSD using libgcc's _Unwind_Backtrace
        // if present
        //
        #if defined(BOOSTER_HAVE_UNWIND_BACKTRACE) && ( defined(__FreeBSD__) || defined(__sun) || defined(__APPLE__) ) 

        namespace {
            struct trace_data {
                void **array;
                int reminder;
                int total;
            };
            extern "C" {
                static int booster_stacktrace_callback(void *context,void *cookie)
                {
                    trace_data *d = static_cast<trace_data *>(cookie);
                    if(d->reminder > 0) {
                        d->reminder --;
                        d->array[d->total++] = _Unwind_GetIP(context);
                    }
                    return 0;
                }
            }
        }

        int trace(void **array,int n)
        {
            trace_data d = { array, n , 0};
            _Unwind_Backtrace(booster_stacktrace_callback,&d);
            return d.total;
        }

        #elif defined(BOOSTER_HAVE_EXECINFO)
        
        int trace(void **array,int n)
        {
            return :: backtrace(array,n);
        }
        
        #elif defined(BOOSTER_MSVC) && _MSC_VER > 1500 

        int trace(void **array,int n)
        {
            if(n>=63)
                n=62;
            return RtlCaptureStackBackTrace(0,n,array,0);
        }
        
        #elif defined(BOOSTER_WIN32)

        extern "C" {
            typedef unsigned short (WINAPI *capture_func_type)(unsigned long, unsigned long ,void **,unsigned long *);
            static capture_func_type capture_stack_trace;
            static bool capture_stack_trace_loaded;
        }

        int trace(void **array,int n)
        {
            if(capture_stack_trace_loaded && !capture_stack_trace)
                return 0;
            if(!capture_stack_trace_loaded) {
                HMODULE h = GetModuleHandle("kernel32.dll");
                if(!h) {
                    capture_stack_trace_loaded = true;
                    return 0;
                }
                capture_stack_trace = (capture_func_type)GetProcAddress(h,"RtlCaptureStackBackTrace");
                capture_stack_trace_loaded = true;
            }
            
            if(!capture_stack_trace)
                return 0;
            
            if(n>=63)
                n=62;

            return capture_stack_trace(0,n,array,0);
        }

        #else

        int trace(void ** /*array*/,int /*n*/)
        {
            return 0;
        }

        #endif
        
        #if defined(BOOSTER_HAVE_DLADDR) && defined(BOOSTER_HAVE_ABI_CXA_DEMANGLE)
        
        std::string get_symbol(void *ptr)
        {
            if(!ptr)
                return std::string();
            std::ostringstream res;
            res.imbue(std::locale::classic());
            res << ptr<<": ";
            Dl_info info = Dl_info();
            if(dladdr(ptr,&info) == 0) {
                res << "???";
            }
            else {
                if(info.dli_sname) {
                    int status = 0;
                    char *demangled = abi::__cxa_demangle(info.dli_sname,0,0,&status);
                    if(demangled) {
                        res << demangled;
                        free(demangled);
                    }
                    else {
                        res << info.dli_sname;
                    }
                }
                else {
                    res << "???";
                }

                unsigned offset = (char *)ptr - (char *)info.dli_saddr;
                res << std::hex <<" + 0x" << offset ;

                if(info.dli_fname)
                    res << " in " << info.dli_fname;
            }
           return res.str();
        }

        std::string get_symbols(void *const *addresses,int size)
        {
            std::string res;
            for(int i=0;i<size;i++) {
                std::string tmp = get_symbol(addresses[i]);
                if(!tmp.empty()) {
                    res+=tmp;
                    res+='\n';
                }
            }
            return res;
        }
        void write_symbols(void *const *addresses,int size,std::ostream &out)
        {
            for(int i=0;i<size;i++) {
                std::string tmp = get_symbol(addresses[i]);
                if(!tmp.empty()) {
                    out << tmp << '\n';
                }
            }
            out << std::flush;
        }

        #elif defined(BOOSTER_HAVE_EXECINFO)
        std::string get_symbol(void *address)
        {
            char ** ptr = backtrace_symbols(&address,1);
            try {
                if(ptr == 0)
                    return std::string();
                std::string res = ptr[0];
                free(ptr);
                ptr = 0;
                return res;
            }
            catch(...) {
                free(ptr);
                throw;
            }
        }
        
        std::string get_symbols(void * const *address,int size)
        {
            char ** ptr = backtrace_symbols(address,size);
            try {
                if(ptr==0)
                    return std::string();
                std::string res;
                for(int i=0;i<size;i++) {
                    res+=ptr[i];
                    res+='\n';
                }
                free(ptr);
                ptr = 0;
                return res;
            }
            catch(...) {
                free(ptr);
                throw;
            }
        }

        
        void write_symbols(void *const *addresses,int size,std::ostream &out)
        {
            char ** ptr = backtrace_symbols(addresses,size);
            try {
                if(ptr==0)
                    return;
                for(int i=0;i<size;i++)
                    out << ptr[i] << '\n';
                free(ptr);
                ptr = 0;
                out << std::flush;
            }
            catch(...) {
                free(ptr);
                throw;
            }
        }
        
        #elif defined(BOOSTER_MSVC)
        
        namespace {
            HANDLE hProcess = 0;
            bool syms_ready = false;
            
            void init()
            {
                if(hProcess == 0) {
                    hProcess = GetCurrentProcess();
                    SymSetOptions(SYMOPT_DEFERRED_LOADS);

                    if (SymInitialize(hProcess, NULL, TRUE))
                    {
                        syms_ready = true;
                    }
                }
            }
        }
        
        std::string get_symbol(void *ptr)
        {
            if(ptr==0)
                return std::string();
            init();
            std::ostringstream ss;
            ss << ptr;
            if(syms_ready) {
                DWORD64  dwDisplacement = 0;
                DWORD64  dwAddress = (DWORD64)ptr;

                std::vector<char> buffer(sizeof(SYMBOL_INFO) + MAX_SYM_NAME);
                PSYMBOL_INFO pSymbol = (PSYMBOL_INFO)&buffer.front();

                pSymbol->SizeOfStruct = sizeof(SYMBOL_INFO);
                pSymbol->MaxNameLen = MAX_SYM_NAME;

                if (SymFromAddr(hProcess, dwAddress, &dwDisplacement, pSymbol))
                {
                    ss <<": " << pSymbol->Name << std::hex << " +0x" << dwDisplacement;
                }
                else
                {
                    ss << ": ???";
                }
            }
            return ss.str();
        }

        std::string get_symbols(void *const *addresses,int size)
        {
            std::string res;
            for(int i=0;i<size;i++) {
                std::string tmp = get_symbol(addresses[i]);
                if(!tmp.empty()) {
                    res+=tmp;
                    res+='\n';
                }
            }
            return res;
        }
        void write_symbols(void *const *addresses,int size,std::ostream &out)
        {
            for(int i=0;i<size;i++) {
                std::string tmp = get_symbol(addresses[i]);
                if(!tmp.empty()) {
                    out << tmp << '\n';
                }
            }
            out << std::flush;
        }

        #elif defined(BOOSTER_WIN32)
        namespace {
            struct module_info {
                void *ptr;
                void *mptr;
                std::string name;
            };

            void get_modules_info(void *const *addrs,int n,module_info *inf)
            {
                std::vector<HMODULE> mods(1024);
                
                HANDLE hProc = GetCurrentProcess();
                DWORD size=0;
                EnumProcessModules(hProc,&mods[0],sizeof(HMODULE)*mods.size(),&size);
                size /= sizeof(HMODULE);

                std::vector<MODULEINFO> info(size,MODULEINFO());
                std::vector<std::string> names(size);

                for(unsigned i=0;i<size;i++) {
                    GetModuleInformation(hProc,mods[i],&info[i],sizeof(MODULEINFO));
                }
                for(int i=0;i<n;i++) {
                    inf[i].ptr=addrs[i];
                    inf[i].mptr = 0;
                    for(unsigned j=0;j<size;j++) {
                        char *addr = (char*)(addrs[i]);
                        char *maddr = (char*)(info[j].lpBaseOfDll);
                        if(maddr <= addr && addr < maddr + info[j].SizeOfImage) {
                            inf[i].mptr = maddr;
                            if(names[j].empty()) {
                                char module_name[256];
                                GetModuleBaseNameA(hProc,mods[j],module_name,sizeof(module_name));
                                names[j]=module_name;
                            }
                            inf[i].name=names[j];
                            break;
                        }
                    }
                    if(inf[i].name.empty())
                        inf[i].name="???";
                }
            };
        } // anon
        
        std::string get_symbol(void *ptr)
        {
            module_info inf;
            get_modules_info(&ptr,1,&inf);
            std::ostringstream ss;
            ss<< inf.ptr<<" in "<<inf.name << " at " << inf.mptr;
            return ss.str();
        }
        
        std::string get_symbols(void *const *ptrs,int size)
        {
            std::ostringstream ss;
            write_symbols(ptrs,size,ss);
            return ss.str();
        }
        
        void write_symbols(void *const *addresses,int size,std::ostream &out)
        {
            if(size==0)
                return;
            std::vector<module_info> inf(size);
            get_modules_info(addresses,size,&inf.front());
            for(int i=0;i<size;i++) {
                out << inf[i].ptr<<" in "<<inf[i].name << " at " << inf[i].mptr << '\n';
            }
            out << std::flush;
        }

        #else

        std::string get_symbol(void *ptr)
        {
            if(!ptr)
                return std::string();
            std::ostringstream res;
            res.imbue(std::locale::classic());
            res << ptr;
            return res.str();
        }

        std::string get_symbols(void *const *ptrs,int size)
        {
            if(!ptrs)
                return std::string();
            std::ostringstream res;
            res.imbue(std::locale::classic());
            write_symbols(ptrs,size,res);
            return res.str();
        }

        void write_symbols(void *const *addresses,int size,std::ostream &out)
        {
            for(int i=0;i<size;i++) {
                if(addresses[i]!=0)
                    out << addresses[i]<<'\n';
            }
            out << std::flush;
        }

        #endif

    } // stack_trace

} // booster

#endif
// vim: tabstop=4 expandtab shiftwidth=4 softtabstop=4

