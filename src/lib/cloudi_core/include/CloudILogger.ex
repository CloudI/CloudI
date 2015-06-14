#-*-Mode:ruby;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=ruby fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
###
###------------------------------------------------------------------------
###
### BSD LICENSE
### 
### Copyright (c) 2014, Michael Truog <mjtruog at gmail dot com>
### All rights reserved.
### 
### Redistribution and use in source and binary forms, with or without
### modification, are permitted provided that the following conditions are met:
### 
###     * Redistributions of source code must retain the above copyright
###       notice, this list of conditions and the following disclaimer.
###     * Redistributions in binary form must reproduce the above copyright
###       notice, this list of conditions and the following disclaimer in
###       the documentation and/or other materials provided with the
###       distribution.
###     * All advertising materials mentioning features or use of this
###       software must display the following acknowledgment:
###         This product includes software developed by Michael Truog
###     * The name of the author may not be used to endorse or promote
###       products derived from this software without specific prior
###       written permission
### 
### THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
### CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
### INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
### OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
### DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
### CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
### SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
### BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
### SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
### INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
### WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
### NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
### OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
### DAMAGE.
###
###------------------------------------------------------------------------

defmodule CloudILogger do

    defmacro log_fatal(format, args) do
        quote do
            :cloudi_core_i_logger_interface.fatal(__MODULE__, __ENV__.line,
                                                  unquote(format),
                                                  unquote(args))
        end
    end

    defmacro log_error(format, args) do
        quote do
            :cloudi_core_i_logger_interface.error(__MODULE__, __ENV__.line,
                                                  unquote(format),
                                                  unquote(args))
        end
    end

    defmacro log_warn(format, args) do
        quote do
            :cloudi_core_i_logger_interface.warn(__MODULE__, __ENV__.line,
                                                 unquote(format),
                                                 unquote(args))
        end
    end

    defmacro log_info(format, args) do
        quote do
            :cloudi_core_i_logger_interface.info(__MODULE__, __ENV__.line,
                                                 unquote(format),
                                                 unquote(args))
        end
    end

    defmacro log_debug(format, args) do
        quote do
            :cloudi_core_i_logger_interface.debug(__MODULE__, __ENV__.line,
                                                  unquote(format),
                                                  unquote(args))
        end
    end

    defmacro log_trace(format, args) do
        quote do
            :cloudi_core_i_logger_interface.trace(__MODULE__, __ENV__.line,
                                                  unquote(format),
                                                  unquote(args))
        end
    end

    defmacro log_fatal_apply(f, a) do
        quote do
            :cloudi_core_i_logger_interface.fatal_apply(unquote(f),
                                                        unquote(a))
        end
    end

    defmacro log_error_apply(f, a) do
        quote do
            :cloudi_core_i_logger_interface.error_apply(unquote(f),
                                                        unquote(a))
        end
    end

    defmacro log_warn_apply(f, a) do
        quote do
            :cloudi_core_i_logger_interface.warn_apply(unquote(f),
                                                       unquote(a))
        end
    end

    defmacro log_info_apply(f, a) do
        quote do
            :cloudi_core_i_logger_interface.info_apply(unquote(f),
                                                       unquote(a))
        end
    end

    defmacro log_debug_apply(f, a) do
        quote do
            :cloudi_core_i_logger_interface.debug_apply(unquote(f),
                                                        unquote(a))
        end
    end

    defmacro log_trace_apply(f, a) do
        quote do
            :cloudi_core_i_logger_interface.trace_apply(unquote(f),
                                                        unquote(a))
        end
    end

    defmacro log_fatal_apply(m, f, a) do
        quote do
            :cloudi_core_i_logger_interface.fatal_apply(unquote(m),
                                                        unquote(f),
                                                        unquote(a))
        end
    end

    defmacro log_error_apply(m, f, a) do
        quote do
            :cloudi_core_i_logger_interface.error_apply(unquote(m),
                                                        unquote(f),
                                                        unquote(a))
        end
    end

    defmacro log_warn_apply(m, f, a) do
        quote do
            :cloudi_core_i_logger_interface.warn_apply(unquote(m),
                                                       unquote(f),
                                                       unquote(a))
        end
    end

    defmacro log_info_apply(m, f, a) do
        quote do
            :cloudi_core_i_logger_interface.info_apply(unquote(m),
                                                       unquote(f),
                                                       unquote(a))
        end
    end

    defmacro log_debug_apply(m, f, a) do
        quote do
            :cloudi_core_i_logger_interface.debug_apply(unquote(m),
                                                        unquote(f),
                                                        unquote(a))
        end
    end

    defmacro log_trace_apply(m, f, a) do
        quote do
            :cloudi_core_i_logger_interface.trace_apply(unquote(m),
                                                        unquote(f),
                                                        unquote(a))
        end
    end

    defmacro function() do
        quote do
            :erlang.element(2,
                :erlang.element(2,
                    :erlang.process_info(:erlang.self(), current_function)))
        end
    end

end
