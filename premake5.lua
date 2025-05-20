-- premake5.lua
--
-- Copyright (c) 2021-2025 Xiongfei Shi
--
-- Author: Xiongfei Shi <xiongfei.shi(a)icloud.com>
-- License: Apache-2.0
--
-- https://github.com/shixiongfei/ape
--

workspace ( "ape" )
  configurations { "Release", "Debug" }
  platforms { "x64", "x32" }

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

    filter ( "configurations:Release" )
      optimize "On"
      defines { "NDEBUG", "_NDEBUG" }

    filter ( "configurations:Debug" )
      symbols "On"
      defines { "DEBUG", "_DEBUG" }

    filter ( "action:vs*" )
      defines { "WIN32", "_WIN32", "_WINDOWS",
                "_CRT_SECURE_NO_WARNINGS", "_CRT_SECURE_NO_DEPRECATE",
                "_CRT_NONSTDC_NO_DEPRECATE", "_WINSOCK_DEPRECATED_NO_WARNINGS" }

    filter ( "action:gmake" )
      warnings  "Default" --"Extra"

    filter { "action:gmake", "system:macosx" }
      defines { "__APPLE__", "__MACH__", "__MRC__", "macintosh" }

    filter { "action:gmake", "system:linux" }
      defines { "__linux__" }

    filter { "action:gmake", "system:bsd" }
      defines { "__BSD__" }
