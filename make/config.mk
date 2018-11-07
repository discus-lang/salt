
# -- Packages -----------------------------------------------------------------
# Haskell packages that need to be installed to build the system.
SALT_PACKAGES := \
	-hide-all-packages \
        -package base \
        -package deepseq \
        -package array \
        -package text \
        -package directory \
        -package bytestring \
        -package containers \
        -package filepath \
        -package inchworm \
        -package parsec \
        -package pretty-show

WAR_PACKAGES := \
        -hide-all-packages \
        -package base \
        -package text \
        -package directory \
        -package filepath \
        -package containers \
        -package stm \
        -package process \
        -package buildbox


# -- Language -----------------------------------------------------------------
#   We have these set globally to keep track of what we're using,
#   and so we don't need to list features in every source file.
#
#   The globally enabled features are mostly syntactic sugar,
#   we don't enable advanced GHC type system extensions globally
#
GHC_LANGUAGE := \
	-XLambdaCase \
	-XRankNTypes \
        -XViewPatterns \
        -XInstanceSigs \
	-XBangPatterns \
        -XTupleSections \
        -XPatternGuards \
        -XNamedFieldPuns \
        -XPackageImports \
        -XConstraintKinds \
        -XPatternSynonyms \
	-XFlexibleContexts \
        -XParallelListComp \
	-XOverloadedStrings \
        -XFlexibleInstances \
	-XStandaloneDeriving \
        -XExplicitNamespaces \
        -XScopedTypeVariables \
	-XMultiParamTypeClasses \
        -XDuplicateRecordFields \
        -XFunctionalDependencies \
	-XExistentialQuantification \
	-XNoMonomorphismRestriction


# -- Warnings -----------------------------------------------------------------
GHC_WARNINGS := \
        -fwarn-identities \
        -fwarn-deprecations \
        -fwarn-unused-binds \
	-fwarn-unused-imports \
        -fwarn-unused-matches \
        -fwarn-wrong-do-bind \
	-fwarn-hi-shadowing \
	-fwarn-type-defaults \
        -fwarn-name-shadowing \
	-fwarn-duplicate-exports \
        -fwarn-missing-fields \
	-fwarn-overlapping-patterns \
	-fwarn-incomplete-patterns \
        -fwarn-name-shadowing \
	-fwarn-unrecognised-pragmas \
	-fwarn-monomorphism-restriction \
        -fno-warn-missing-methods \
        -fno-warn-missing-signatures \
        -fno-warn-missing-local-signatures \
        -fno-warn-orphans \
        -fno-warn-simplifiable-class-constraints


# -- Build tools --------------------------------------------------------------
# Number of jobs to use during make.
THREAD          = 3

# Use ghc in the current path by default.
GHC             = ghc
GHC_FLAGS       = -Werror -O0 -j3

# Override config with local config if it exists.
#  If you want to change the THREADs or GHC variables then add local
#  make/config-override.mk file which is not under version control.
-include make/config-override.mk


