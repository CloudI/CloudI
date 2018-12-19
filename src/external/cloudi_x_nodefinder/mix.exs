#-*-Mode:elixir;coding:utf-8;tab-width:2;c-basic-offset:2;indent-tabs-mode:()-*-
# ex: set ft=elixir fenc=utf-8 sts=2 ts=2 sw=2 et nomod:

defmodule Nodefinder.Mixfile do
  use Mix.Project

  def project do
    [app: :nodefinder,
     version: "1.7.5",
     language: :erlang,
     erlc_options: [
       {:d, :erlang.list_to_atom('ERLANG_OTP_VERSION_' ++ :erlang.system_info(:otp_release))},
       :debug_info,
       :warnings_as_errors,
       :strict_validation,
       :warn_bif_clash,
       :warn_deprecated_function,
       :warn_export_all,
       :warn_export_vars,
       :warn_exported_vars,
       :warn_obsolete_guard,
       :warn_shadow_vars,
       :warn_unused_import,
       :warn_unused_function,
       :warn_unused_record,
       :warn_unused_vars],
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
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/okeuday/nodefinder"}]
   end
end
