const path = require('path');
const HtmlWebPackPlugin = require('html-webpack-plugin');

module.exports = {
    mode: 'development',
    entry: {
        app: './src/index.ts',
        'editor.worker': 'monaco-editor/esm/vs/editor/editor.worker.js',
    },
    resolve: {
        extensions: ['.ts', '.js']
    },
    output: {
        globalObject: 'self',
        filename: '[name].bundle.js',
        path: path.resolve(__dirname, 'dist')
    },
    module: {
        rules: [
            {
                test: /\.ts?$/,
                use: 'ts-loader',
                exclude: /node_modules/
            },
            {
                test: /\.css$/,
                use: ['style-loader', 'css-loader']
            },
            {
                test: /\.ttf$/,
                use: ['file-loader']
            }
        ]
    },
    plugins: [
        new HtmlWebPackPlugin({
            title: 'Monaco Editor Sample'
        })
    ]
};
