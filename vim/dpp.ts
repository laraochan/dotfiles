import {
  BaseConfig,
  type ConfigArguments,
  type ConfigReturn,
} from "jsr:@shougo/dpp-vim/config";
import { type Toml } from "jsr:@shougo/dpp-ext-toml";

export class Config extends BaseConfig {
  override async config(args: ConfigArguments): Promise<ConfigReturn> {
    args.contextBuilder.setGlobal({
      protocols: ["git"],
    })

    const [context, options] = await args.contextBuilder.get(args.denops);

    const tomlPlugins: Toml = await args.dpp.extAction(
      args.denops,
      context,
      options,
      "toml",
      "load",
      {
        path: await args.denops.call("expand", "~/.config/vim/plugins.toml"),
      },
    );

    return {
      plugins: tomlPlugins.plugins || [],
    };
  }
}
