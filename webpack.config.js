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
		headers: {
		  'Access-Control-Allow-Origin': '*',
		  'Access-Control-Allow-Headers': '*',
		},

    },
	devtool: production ? false : "eval-source-map",
    module: {
        rules: [{
            test: /\.fs(x|proj)?$/,
            use: {
				loader: "fable-loader",
				options: {
					cli: {
						path: "C:\\Users\\Schorsch\\Development\\Fable\\src\\Fable.Cli"
					}
				}
			}
        }]
    }
}