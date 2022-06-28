/* Copyright (C) 1992-2022 Free Software Foundation, Inc.
   Copyright The GNU Toolchain Authors.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <https://www.gnu.org/licenses/>.  */

#pragma line 19
#pragma line 20
#ifndef	_SYS_CDEFS_H
#define	_SYS_CDEFS_H	1

#pragma line 24

/* We are almost always included from features.h. */
#ifndef _FEATURES_H
# include <features.h>
#endif

#pragma line 31

/* The GNU libc does not support any K&R compilers or the traditional mode
   of ISO C compilers anymore.  Check for some of the combinations not
   supported anymore.  */
#if defined __GNUC__ && !defined __STDC__
# error "You need a ISO C conforming compiler to use the glibc headers"
#endif

/* Some user header file might have defined this before.  */
#undef	__P
#undef	__PMT

/* Compilers that lack __has_attribute may object to
       #if defined __has_attribute && __has_attribute (...)
   even though they do not need to evaluate the right-hand side of the &&.
   Similarly for __has_builtin, etc.  */
#if (defined __has_attribute \
     && (!defined __clang_minor__ \
         || 3 < __clang_major__ + (5 <= __clang_minor__)))
# define __glibc_has_attribute(attr) __has_attribute (attr)
#else
# define __glibc_has_attribute(attr) 0
#endif
#ifdef __has_builtin
# define __glibc_has_builtin(name) __has_builtin (name)
#else
# define __glibc_has_builtin(name) 0
#endif
#ifdef __has_extension
# define __glibc_has_extension(ext) __has_extension (ext)
#else
# define __glibc_has_extension(ext) 0
#endif

#if defined __GNUC__ || defined __clang__

/* All functions, except those with callbacks or those that
   synchronize memory, are leaf functions.  */
#pragma line 70
#endif
#endif
