defmodule IDNA.Mixfile do
  use Mix.Project

  def project do
    [app: :idna,
     version: "1.2.0",
     description: "A pure Erlang IDNA implementation",
     package: package]
  end

  defp package do
    [files: ~w(src priv ebin Makefile Emakefile README.md License.txt),
     contributors: ["Benoit Chesneau", "Tim Fletcher"],
     licenses: ["MIT"],
     links: [{"GitHub", "https://github.com/benoitc/erlang-idna/"}]]
  end
end
