import {
	BaseConfig,
	type ConfigArguments,
	type ConfigReturn,
	type MultipleHook
} from "jsr:@shougo/dpp-vim@~6.0.0/config";
import { mergeFtplugins } from "jsr:@shougo/dpp-vim@~6.0.0/utils";
import {
	type Plugin,
	type ExtOptions
} from "jsr:@shougo/dpp-vim@~6.0.0/types";
import {
	Ext as TomlExt,
	type Params as TomlParams
} from "jsr:@shougo/dpp-ext-toml@~2.0.1";
import {
	Ext as LazyExt,
	type Params as LazyParams,
	type LazyMakeStateResult
} from "jsr:@shougo/dpp-ext-lazy@~2.0.1";

import * as fn from "jsr:@denops/std@~8.2.0/function";

export class Config extends BaseConfig {
	override async config(args: ConfigArguments): Promise<ConfigReturn> {
		args.contextBuilder.setGlobal({
			protocols: ['git'],
		});

		const [context, options] = await args.contextBuilder.get(args.denops);
		
		const recordPlugins: Record<string, Plugin> = {};
		const ftplugins: Record<string, string> = {};
		const hooksFiles: string[] = [];
		let multipleHooks: MultipleHook[] = [];

		const [tomlExt, tomlOptions, tomlParams] = await args.denops.dispatcher.getExt('toml') as [TomlExt | undefined, ExtOptions, TomlParams];
		if (tomlExt) {
			const action = tomlExt.actions.load;
			
			const tomlPromises = [
				{ path: '~/.config/vim/plugins.toml', lazy: false },
			].map(async tomlFile => {
				return await action.callback({
					denops: args.denops,
					context,
					options,
					protocols: ['git'],
					extOptions: tomlOptions,
					extParams: tomlParams,
					actionParams: {
						path: await fn.expand(args.denops, tomlFile.path),
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

		const [lazyExt, lazyOptions, lazyParams] = await args.denops.dispatcher.getExt('lazy') as [LazyExt | undefined, ExtOptions, LazyParams];
		let lazyResult: LazyMakeStateResult | undefined = undefined;
		if (lazyExt) {
			const action = lazyExt.actions.makeState;

			lazyResult = await action.callback({
				denops: args.denops,
				context,
				options,
				protocols: ['git'],
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
