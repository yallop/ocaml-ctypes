#ifndef CTYPES_POSIX_COMPATIBILITY_H
#define CTYPES_POSIX_COMPATIBILITY_H

// Placing this first because for Windows critical defines like `WIN32_LEAN_AND_MEAN`
// and critical includes like `<winsock2.h>` are used. These must occur before pulling
// in any other Windows headers that would transitively include `<windows.h>` so that
// Windows does not have conflicts between Winsock 1 and Winsock 2 APIs.
// For consistency we're including this on all platforms that need POSIX code.
#include <caml/unixsupport.h>

#ifdef _MSC_VER
# include <basetsd.h>
# include <WTypesbase.h>
/* The following types are not defined by msvc. */
/* _chmod for Microsoft has second arg (the mode) as int. https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/chmod-wchmod?view=msvc-160 */
typedef int mode_t;
/* _getpid for Microsoft is int. https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/getpid?redirectedfrom=MSDN&view=msvc-160 */
typedef int pid_t;
typedef SSIZE_T ssize_t;
/* usleep() is deprecated POSIX. QueryPerformanceCounter is MS equivalent https://docs.microsoft.com/en-us/windows/win32/api/profileapi/nf-profileapi-queryperformancecounter
   but its interval type is a struct called LARGE_INTEGER with a single LONGLONG member.
   Developers, if they continue to want to use it and work on Windows, will need to wrap their own usleep() around QueryPerformanceCounter. */
typedef LONGLONG useconds_t;
#endif

#endif /* CTYPES_POSIX_COMPATIBILITY_H */
