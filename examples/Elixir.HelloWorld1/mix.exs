defmodule HelloWorld1.Mixfile do
    use Mix.Project

    def project do
        [app: :Elixir.HelloWorld1,
         version: "1.3.2",
         elixir: "~> 0.14.3",
         elixirc_paths: ["lib/",
                         "/usr/local/lib/cloudi-1.3.2/lib/cloudi_core-1.3.2/include/"],
         deps: deps]
    end

    # Configuration for the OTP application
    #
    # Type `mix help compile.app` for more information
    def application do
        [applications: []]
    end

    # Dependencies can be hex.pm packages:
    #
    #   {:mydep, "~> 0.3.0"}
    #
    # Or git/path repositories:
    #
    #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1"}
    #
    # Type `mix help deps` for more examples and options
    defp deps do
        []
    end
end
