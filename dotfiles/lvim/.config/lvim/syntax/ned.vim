" Vim syntax file
" Language:	Omnet++ NED
" Maintainer:	Timo Teifel <timo@teifel-net.de>
" URL:          www.teifel.net/projects/vim
" Last Change:	November 28, 2005
" Version:      0.2
" Released under the terms of the GNU/GPL licence v2
"
" Very basic syntax file for NED files by OMNeT++
" does not know any regions/areas yet. Maybe I will implement
" this when I learned how to do it ;)

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syntax case match

syn match nedValidNumber        "\d\+" nextgroup=nedValidTimeUnits skipwhite
syn match nedValidNumber        "\d\+\.\d\+" nextgroup=nedValidTimeUnits skipwhite
syn match nedValidNumber        "\d*\.\d\+" nextgroup=nedValidTimeUnits skipwhite

syn match nedValidTimeUnits     "ns" 
syn match nedValidTimeUnits     "m"
syn match nedValidTimeUnits     "ms"
syn match nedValidTimeUnits     "s"
syn match nedValidTimeUnits     "h"
syn match nedValidTimeUnits     "d"

syn keyword nedConditional      if
syn keyword nedInclude          import
syn keyword nedRepeat           for do endfor
syn keyword nedType             string numeric bool xml char const
syn keyword nedBoolean          true false
syn match nedIdentifier         "[a-zA-Z_][0-9a-zA-Z_]*"
syn match nedComment            "\/\/.*"

syn keyword nedChannel          channel endchannel
syn keyword nedChannelOptions   delay error datarate
syn keyword nedSimple           simple endsimple
syn keyword nedModule           module endmodule
syn match nedSubModules         "submodules:"he=e-1
syn match nedModuleOptions      "display:"he=e-1

syn keyword nedNetwork          network endnetwork

syn match nedSimpleDefinition   "parameters:"he=e-1
syn match nedSimpleDefinition   "gates:"he=e-1

"syn region nedGates              start="gates:" end="endsimple" contains=nedGateOptions
syn match nedGateOptions        "in:"he=e-1 " contained
syn match nedGateOptions        "out:"he=e-1 " contained

syn region nedString		    start=/"/ skip=/\\"/ end=/"/
syn keyword nedReservedWords    connections gatesizes nocheck ref ancestor like input  
syn keyword nedFunctions        xmldoc sizeof uniform exponential normal truncnormal
syn keyword nedFunctions        gamma_d beta erlang_k chi_square student_t cauchy
syn keyword nedFunctions        triang lognormal weibull pareto_shifted intuniform
syn keyword nedFunctions        bernoulli binomial geometric negbinomial poisson

if version >= 508 || !exists("did_ned_syn_inits")
    if version < 508
      let did_ned_syn_inits = 1
      command -nargs=+ HiLink hi link <args>
    else
      command -nargs=+ HiLink hi def link <args>
    endif
    HiLink nedComment                     Comment
    HiLink nedConditional                 Conditional
    HiLink nedRepeat                      Repeat
    HiLink nedIdentifier                  Identifier
    HiLink nedValidTimeUnits              Constant
    HiLink nedValidNumber                 Number
    HiLink nedInclude                     Include
    HiLink nedType                        Type
    HiLink nedBoolean                     Boolean
    HiLink nedChannel                     Keyword
    HiLink nedChannelOptions              Keyword
    HiLink nedSimple                      Keyword
    HiLink nedSimpleDefinition            Keyword
    HiLink nedGateOptions                 Keyword
    HiLink nedString                      String
    HiLink nedModule                      Keyword
    HiLink nedSubModules                  Keyword
    HiLink nedModuleOptions               Keyword
    HiLink nedNetwork                     Keyword
    HiLink nedEndNetwork                  Keyword
    HiLink nedReservedWords               Keyword
    HiLink nedFunctions                   Keyword
    delcommand HiLink
endif

let b:current_syntax = "ned"
