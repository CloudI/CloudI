# Changes

## Current Master

**If you run into trouble, please try the 'stable' branch.**

	git checkout origin/stable

#### Jun 21 2013 hd | doc
* Remove utf-8 encoding of binary data
  *Rationale*: The user must ensure that data passed to the MySQL
  connection is already encoded in the right utf-8 format. The socket
  doesn't know about encoding. In turn, one can again send binary BLOB
  data over the driver.
* Fixes many small errors by merging two major Emysql branches

#### Jun 21 2013 hd | doc
* Fixed abuse of ETS table to store Mysql results
* Fixed floating point encoding in the driver base
* Support using atoms as prepared statements
* Optimize the code path around SETing parameters
* Fixed bug in receiving packet bodies which would lead to conn. stream corruption
* Make it possible to use configurable TCP timeout values
* Fix date encoding of special dates, e.g., year 1

#### Apr 23 2013 hd | doc
* Fixed issue #44: LOAD DATA INFILE. Thanks to Qing Liang / qingliangcn.  
* Added samples and brief description of LOAD DATA INFILE  
* whitespace clean up  

#### Mar 26 2012 hd | Changed UTF-8/Latin-1 conversion
* Magic taken out of automatic encoding  
* Added Common Tests for Latin-1 encoding  
* Added Common Tests for cross encoding (connection and db different)  
* This addressed issue #22, thanks to TheSquad  

Notes for March 26 2012:  

**Incompatible changes for automatic UTF-8 conversion.**  

There are cases where the automatic conversion of parameters to prepared
statements has been changed and now behaves different than before. The
conversion now respects the encoding setting of the connection pool that it
is for, which you assigned when opening the pool.

There are extensive new tests checking as many sensible cases as possible, among
them 40+ 'human readable' cases in the files test/utf8_SUITE, test/latin_SUITE,
test/utf8_to_latindb_SUITE, test/latin_to_utf8db_SUITE. Please refer to these
first in cases where you have doubt about expected behavior in fringe cases.


#### Dec 14 2011 al | Detection of failed statement preparation d8330f035c
* Error had been dropped silently.  
* Merge pull request #16 from binarin/master.  
* Quote sql statement passed when preparing it.   
* Always turn prepare error into exit().  
* Thanks to Alexey Lebedeff / binarin.  

#### Dec 13 2011 hd | Unicode UTF-8 string conversion 287e17b2d3
* Strings passed as lists to prepared statements as parameters are now converted right.  
* Issue #14, automatic conversion of list strings to utf-8.  

#### Dec 13 2011 hd | Common Tests for unicode 287e17b2d3
* Common Test suite for unicode conversion.  

#### Dec 12 2011 es | parsing of numbers in scientific notation c20ff42bbe
* Fixed float notation parsing to work with the form 1e-1 besides 1.0e-1.  
* Thanks to Erik Seres / eseres.  

#### Dec 11 2011 hd | Common Test suites 4fdf9eedf9
* Common Test suites for basic operations, and environment set up.  

#### Oct 27 2011 ol | name collision with RabbitMQ 0fce8c9cc2
* Avoid name collision with RabbitMQ for the connection record.  
* Merge pull request #12 from lsowen/master.  
* Thanks to Logan Owen / lsowen.  

#### Jun 18 2011 pa | PoolId restrictions removed 1deb2063e7
* PoolId restrictions removed to prevent atom overflow  
* Thanks to Patrick Atambo / partoa.  
 
#### May 02 2011 hd | Fix of Connection Auto-Recover 3db0e45319
* Proposal for fix of issue #7 after quite some research.  
* The fix goes deep, so to be safe, tagging of previous master as 'stable'   branch.
* Tagging of dev branches for issue #7 and #9.  
* Support by Seven Du / seven1240 and Brendon Hogger / brendonh  

#### Mar 23 2011 jm | Protocol Bug Fix 8a3c6dc123
* Merge branch 'fix_pattern_mismatch'  
* Thanks to Joel Meyer / JoelPM  

#### Mar 17 2011 es | Added Connection Auto-Recover 3c9d0fe54b
* Added capability to reopen a timedout connection when one is encountered.  
* Thanks to Erik Seres / eseres  

#### Mar 06 2011 hd | MySQL 4.1 protocol + password plugin 63c34c4bb8
* Improved and extended support for client/server 4.0 and 4.1 protocol.  
* Fix issue #6: Added support for MySQL server's password plugin.  
* Added emysql_conn:hstate/1 to return human readable server state flag.  

#### Feb 18 2011 hd | Docs e72142cb13
* Docs for module emysql.erl complete  
* Fix issue #5: make dep of include/

## 0.1

#### Jan 24 2011 hd | Stored Procedures + Docs 
* Fixed stored procedures
* Added docs

#### Jun 12 2010 hd | Samples + History
* Added samples and build scripts  
* Added History of MySQL driver forks  

* Correct handling of stored procedures result packages  
* Module emysql_tcp extended to handle the flag SERVER_MORE_RESULTS_EXIST correctly  
* Error packages parse additional sql status flag of mysql 4.1 format.  
* Additional example for stored procedures  
* Renaming of samples  
* Much extended docs  
