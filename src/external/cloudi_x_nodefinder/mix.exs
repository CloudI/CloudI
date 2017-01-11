defmodule Nodefinder.Mixfile do
  use Mix.Project

  def project do
    [app: :nodefinder,
     version: "1.6.0",
     language: :erlang,
     description: description(),
     package: package(),
     deps: deps()]
  end

  def application do
    [applications: [
       :inets,
       :xmerl,
       :crypto,
       :public_key,
       :ssl],
     mod: {:nodefinder_app, []},
     registered: [
       :nodefinder_ec2,
       :nodefinder_multicast,
       :nodefinder_sup],
     env: [
       node_type: :visible]]
  end

  defp deps do
    []
  end

  defp description do
    "Strategies For Automatic Node Discovery"
  end

  defp package do
    [files: ~w(src doc rebar.config AUTHORS LICENSE ChangeLog README.markdown),
     maintainers: ["Michael Truog", "Paul Mineiro"],
     licenses: ["BSD"],
     links: %{"GitHub" => "https://github.com/okeuday/nodefinder"}]
   end
end
