// Note this only includes basic configuration for development mode.
// For a more comprehensive configuration check:
// https://github.com/fable-compiler/webpack-config-template

var path = require("path");

var production = process.argv.indexOf("-p") >= 0;

module.exports = {
    mode: production ? "production" : "development",
    entry: {
        demo:       "./src/PointCloudViewer/Demo/Demo.fsproj",
        worker:     "./src/PointCloudViewer/Worker/Worker.fsproj",
        importer:   "./src/PointCloudViewer/Importer/Importer.fsproj"
    },
    output: {
        path: path.join(__dirname, "./public"),
        filename: "[name].js",
    },
    devServer: {
        contentBase: "./public",
        port: 8080,
        host: '0.0.0.0',
        headers: {
          'Access-Control-Allow-Origin': '*',
          'Access-Control-Allow-Headers': '*',
        },
        clientLogLevel: 'error',

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