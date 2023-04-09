#lang scribble/manual
@require[@for-label[bdnd
                    racket/base]]

@title{bdnd}
@author{hin}

@defmodule[bdnd]

Compress a directory to a single module. Subdirectories that contain files will be traced. And once a filesystem exception occurs, the process will abort.

PS:users must ensure the filesystem level security because there is no file lock in this package.
