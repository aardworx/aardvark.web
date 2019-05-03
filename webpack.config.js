// Note this only includes basic configuration for development mode.
// For a more comprehensive configuration check:
// https://github.com/fable-compiler/webpack-config-template

var path = require("path");

var production = process.argv.indexOf("-p") >= 0;

module.exports = {
    mode: production ? "production" : "development",
    entry: "./src/Demo/Demo.fsproj",
    output: {
        path: path.join(__dirname, "./public"),
        filename: "bundle.js",
    },
    devServer: {
        contentBase: "./public",
        port: 8080,
		host: '0.0.0.0',
		disableHostCheck: true,
		historyApiFallback: true,
		headers: {
		  "Access-Control-Allow-Methods": "GET, POST, PUT, DELETE, PATCH, OPTIONS",
		  'Access-Control-Allow-Headers': '*',
		  'Access-Control-Allow-Origin': '*',
		},

    },
	devtool: production ? false : "eval-source-map",
    module: {
        rules: [{
            test: /\.fs(x|proj)?$/,
            use: {
				loader: "fable-loader",
				options: {
					define: ["FABLE_QUOTATIONS"],
					cli: {
						path: "./paket-files/fable-compiler/Fable/src/Fable.Cli"
					}
				}
			}
        }]
    }
}