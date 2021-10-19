-- premake5.lua
--
-- Copyright (c) 2021 Xiongfei Shi
--
-- Author: Xiongfei Shi <xiongfei.shi(a)icloud.com>
-- License: Apache-2.0
--
-- https://github.com/shixiongfei/ape
--

solution ( "ape" )
  configurations { "Release", "Debug" }
  platforms { "x64" }

  if _ACTION == "clean" then
    os.rmdir(".vs")
    os.rmdir("bin")
    os.rmdir("objs")
    os.remove("ape.VC.db")
    os.remove("ape.sln")
    os.remove("ape.vcxproj")
    os.remove("ape.vcxproj.filters")
    os.remove("ape.vcxproj.user")
    os.remove("ape.make")
    os.remove("Makefile")
    return
  end

  -- A project defines one build target
  project ( "ape" )
  kind ( "ConsoleApp" )
  language ( "C" )
  files { "./*.h", "./*.c" }
  objdir ( "./objs" )
  targetdir ( "./bin" )
  defines { "_UNICODE" }
  staticruntime "On"

  configuration ( "Release" )
    optimize "On"
    defines { "NDEBUG", "_NDEBUG" }

  configuration ( "Debug" )
    symbols "On"
    defines { "DEBUG", "_DEBUG" }

  configuration ( "vs*" )
    defines { "WIN32", "_WIN32", "_WINDOWS",
              "_CRT_SECURE_NO_WARNINGS", "_CRT_SECURE_NO_DEPRECATE",
              "_CRT_NONSTDC_NO_DEPRECATE", "_WINSOCK_DEPRECATED_NO_WARNINGS" }

  configuration ( "gmake" )
    warnings  "Default" --"Extra"

  configuration { "gmake", "macosx" }
    defines { "__APPLE__", "__MACH__", "__MRC__", "macintosh" }

  configuration { "gmake", "linux" }
    defines { "__linux__" }

  configuration { "gmake", "bsd" }
    defines { "__BSD__" }
