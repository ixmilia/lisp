using System;
using System.Collections.Generic;

namespace IxMilia.Lisp.LanguageServer.Protocol
{
    internal class SemanticTokensBuilder
    {
        private uint _lastLine = 0;
        private uint _lastCharacter = 0;

        private Dictionary<string, uint> _tokenTypeMap = new Dictionary<string, uint>();
        private Dictionary<string, uint> _tokenModifierMap = new Dictionary<string, uint>();

        private List<uint> _data = new List<uint>();

        public SemanticTokensBuilder(string[] tokenTypes, string[] tokenModifiers)
        {
            for (int i = 0; i < tokenTypes.Length; i++)
            {
                _tokenTypeMap.Add(tokenTypes[i], (uint)i);
            }

            for (int i = 0; i < tokenModifiers.Length; i++)
            {
                _tokenModifierMap.Add(tokenModifiers[i], (uint)i);
            }
        }

        public void AddToken(uint line, uint character, uint length, string tokenType, params string[] tokenModifiers)
        {
            var deltaLine = line - _lastLine;
            var deltaCharacter = line == _lastLine
                ? character - _lastCharacter
                : character;
            var tokenTypeValue = _tokenTypeMap[tokenType];
            var tokenModifiersValue = 0u;
            foreach (var tokenModifier in tokenModifiers)
            {
                var bit = _tokenModifierMap[tokenModifier];
                SetBit(ref tokenModifiersValue, bit);
            }

            _data.Add(deltaLine);
            _data.Add(deltaCharacter);
            _data.Add(length);
            _data.Add(tokenTypeValue);
            _data.Add(tokenModifiersValue);

            _lastLine = line;
            _lastCharacter = character;
        }

        public SemanticTokens Build()
        {
            return new SemanticTokens() { Data = _data.ToArray() };
        }

        private void SetBit(ref uint value, uint bit)
        {
            var bitToSet = (uint)(1 << (int)bit);
            var result = value | bitToSet;
            value = result;
        }
    }
}
