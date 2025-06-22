import {
  BaseConfig,
  type ConfigArguments,
  type ConfigReturn,
} from "jsr:@shougo/dpp-vim/config";
import { type Plugin } from "jsr:@shougo/dpp-vim/types";


export class Config extends BaseConfig {
  override async config(args: ConfigArguments): ConfigReturn {
    args.contextBuilder.setGlobal({
      protocols: ["git"],
    });

    const plugins: Plugin[] = [
      { name: "Shougo/ddu.vim" },
    ];

    return {
      plugins: plugins,
    };
  }
}
