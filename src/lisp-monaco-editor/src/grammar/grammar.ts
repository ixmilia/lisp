import * as monaco from 'monaco-editor';
import * as contracts from './contracts';

export const languageGrammar: monaco.languages.IMonarchLanguage = {
    ignoreCase: true,
    defaultToken: '',

    brackets: [
        { open: '(', close: ')', token: 'delimiter.parenthesis' },
        { open: '{', close: '}', token: 'delimiter.curly' },
        { open: '[', close: ']', token: 'delimiter.square' }
    ],

    keywords: contracts.builtInNames,

    constants: ['t', 'nil'],

    tokenizer: {
        root: [
            [/((\+|-)?\d+)(\/(\d+))?/, 'number'], // both integers and ratios
            [/((\+|-)?\d+(\.\d+)?(e(\+|-)?\d+)?)/, 'number.float'],
            [/;.*$/, 'comment'],
            [/#\\./, 'string.escape'], // raw character, e.g.: #\a
            [/#<[^>]*>/, 'invalid'], // unreadable tokens, e.g.: #<unreadable>
            [/"/, { token: 'string.quote', next: '@stringLiteral' }],

            [
                /[a-zA-Z_][a-zA-Z0-9_\-\?\!\*]*/,
                {
                    cases: {
                        '@constants': 'constant',
                        '@keywords': 'keyword',
                        '@default': 'identifier'
                    }
                }
            ]
        ],

        stringLiteral: [
            [/\\./, 'string.escape'], // escaped character
            [/[^\\"]+/, 'string'], // anything that's not an ending quote or backslash
            [/"/, { token: 'string.quote', next: '@pop' }] // ending quote
        ]
    }
};
