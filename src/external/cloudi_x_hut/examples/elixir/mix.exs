defmodule HutExampleElixir.Mixfile do
  use Mix.Project

  def project do
    [app: :hut_example_elixir,
     version: "0.1.0",
     elixir: "~> 1.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  def application do
    [applications: [:logger, :hut_example]]
  end

  defp deps do
    [
      {:hut_example, path: "../basic", compile: "gmake compile_example_elixir"}
    ]
  end
end
