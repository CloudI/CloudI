defmodule HelloWorld1 do

    defmacro log_warn(format, args) do
        quote do
            :cloudi_core_i_logger_interface.warn(__MODULE__, __ENV__.line,
                                                 unquote(format), unquote(args))
        end
    end

    def cloudi_service_init(_args, _prefix, dispatcher) do
        :cloudi_service.subscribe(dispatcher, 'hello_world1/get')
        {:ok, :undefined}
    end

    def cloudi_service_handle_request(_type, _name, _pattern,
                                      _requestinfo, _request,
                                      _timeout, _priority,
                                      _transid, _pid, state, _dispatcher) do
        {:reply, "Hello World!", state}
    end

    def cloudi_service_handle_info(request, state, _dispatcher) do
        log_warn('Unknown info \"~p\"', [request])
        {:noreply, state}
    end

    def cloudi_service_terminate(_reason, _state) do
        :ok
    end
end
