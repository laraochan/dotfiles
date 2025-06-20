import {
  BaseConfig,
  ContextBuilder,
  Dpp,
  Plugin,
} from "https://deno.land/x/dpp_vim@v0.0.5/types.ts";
import { Denops, fn } from "https://deno.land/x/dpp_vim@v0.0.5/deps.ts";

export class Config extends BaseConfig {
  override async config(args: {
    denops: Denops;
    contextBuilder: ContextBuilder;
    basePath: string;
    dpp: Dpp;
  }): Promise<{
    plugins: Plugin[];
    stateLines: string[];
  }> {
    args.contextBuilder.setGlobal({
      protocols: ["git"],
    });

    const plugins: Plugin[]= [
	    {
		    name: "zenbones-theme/zenbones.nvim",
		    repo: "zenbones-theme/zenbones.nvim",
		    depends: "rktjmp/lush.nvim",
	    },
	    {
		    name: "rktjmp/lush.nvim",
		    repo: "rktjmp/lush.nvim",
	    },
    ];

    return {
	    plugins: plugins,
    };
  }
}
