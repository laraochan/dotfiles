import {
	BaseConfig,
	type ConfigArguments,
	type ConfigReturn,
	type MultipleHook,
} from "jsr:@shougo/dpp-vim@5.0.0/config";
import type {
	ExtOptions,
	Plugins,
} from "jsr:@shougo/dpp-vim@5.0.0/types";
import {
	mergeFtplugins,
} from "jsr:@shougo/dpp-vim@5.0.0/utils";
import type {
	Ext as TomlExt,
	Params as TomlParams,
	LazyMakeStateResult,
} from "jsr:@shougo/dpp-ext-toml@2.0.1";
import type {
	Ext as LazyExt,
	Params as LazyParams,
} from "jsr:@shougo/dpp-ext-lazy@2.0.1";

import * as fn from "jsr:@denops/std@8.0.0/function";

export class Config extends BaseConfig {
	override async config(args: ConfigArguments): Promise<ConfigReturn> {
		args.contextBuilder.setGlobal({
			protocols: ["git"],
		});

		const [context, options] = await args.contextBuilder.get(args.denops);

		const recordPlugins: Record<string, Plugin> = {};
		const ftplugins: Record<string, string> = {};
		const hooksFiles: string[] = [];
		let multipleHooks: MultipleHook[] = [];

		const [tomlExt, tomlOptions, tomlParams]: [
			TomlExt | undefined,
			ExtOptions,
			TomlParams,
		] = await args.denops.dispatcher.getExt("toml");
		if (tomlExt) {
			const action = tomlExt.actions.load;

			const tomlPromises = [
				{ path: "~/.config/nvim/toml/dpp.toml", lazy: false },
				{ path: "~/.config/nvim/toml/plugins.toml", lazy: false },
				{ path: "~/.config/nvim/toml/plugins_lazy.toml", lazy: true },
			].map(async (tomlFile) => {
				return await action.callback({
					denops: args.denops,
					context,
					protocols: context.protocols,
					options,
					extOptions: tomlOptions,
					extParams: tomlParams,
					actionParams: {
						path: await fn.expand(args.denops, tomlFile.path) as string,
						options: {
							lazy: tomlFile.lazy,
						},
					},
				});
			});

			const tomls = await Promise.all(tomlPromises);

			for (const toml of tomls) {
				for (const plugin of toml.plugins ?? []) {
					recordPlugins[plugin.name] = plugin;
				}

				if (toml.ftplugins) {
					mergeFtplugins(ftplugins, toml.ftplugins);
				}

				if (toml.multiple_hooks) {
					multipleHooks = multipleHooks.concat(toml.multiple_hooks);
				}

				if (toml.hooks_file) {
					hooksFiles.push(toml.hooks_file);
				}
			}
		}

		const [lazyExt, lazyOptions, lazyParams]: [
			LazyExt | undefined,
			ExtOptions,
			LazyParams,
		] = await args.denops.dispatcher.getExt("lazy");
		let lazyResult: LazyMakeStateResult | undefined = undefined;
		if (lazyExt) {
			const action = lazyExt.actions.makeState;

			const lazyResult = await action.callback({
				denops: args.denops,
				context,
				protocols: context.protocols,
				options,
				extOptions: lazyOptions,
				extParams: lazyParams,
				actionParams: {
					plugins: Object.values(recordPlugins),
				},
			});
		}

		return {
			ftplugins,
			hooksFiles,
			multipleHooks,
			plugins: lazyResult?.plugins ?? [],
			stateLines: lazyResult?.stateLines ?? [],
		};
	}
}
