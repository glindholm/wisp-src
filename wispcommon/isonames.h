/* 
* $Id:$
* Copyright (c) Shell Stream Software LLC. All Rights Reserved.
*/


#ifndef ISONAMES_H
#define ISONAMES_H

#define chmod(file, mode)		_chmod(file, mode)
#define getcwd(buffer, maxlen)		_getcwd(buffer, maxlen)
#define open(filename, oflag, pmode)	_open(filename, oflag, pmode)
#define read(file, buff, count)		_read(file, buff, count)
#define write(file, buff, count)	_write(file, buff, count)
#define close(file)			_close(file)
#define unlink(file)			_unlink(file)
#define access(filename, mode)		_access(filename, mode)
#define getpid()			_getpid()
#define rmdir(dir)			_rmdir(dir)
#define putenv(var)			_putenv(var)
#define lseek(file, offset, origin)	_lseek(file, offset, origin)
#define chdir(path)			_chdir(path)
#define execvp(filename, arglist)	_execvp(filename, arglist)
#define tempnam(dir, prefix)		_tempnam(dir, prefix)
#define fileno(file)			_fileno(file)

#endif /* ISONAMES_H */
