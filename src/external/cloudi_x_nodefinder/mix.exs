defmodule Nodefinder.Mixfile do
  use Mix.Project

  def project do
    [app: :nodefinder,
     version: "1.4.0-rc.4",
     language: :erlang,
     description: description,
     package: package,
     deps: deps]
  end

  defp deps do
    [{:erlcloud, "~> 0.9.1"}]
  end

  defp description do
    "Strategies For Automatic Node Discovery"
  end

  defp package do
    [files: ~w(src doc rebar.config AUTHORS LICENSE ChangeLog README.markdown),
     contributors: ["Michael Truog", "Paul Mineiro"],
     licenses: ["BSD"],
     links: %{"GitHub" => "https://github.com/okeuday/nodefinder"}]
   end
end
