using Xunit;

namespace IxMilia.Lisp.Interactive.Test
{
    public class LispObjectToDeclarationTests
    {
        [Fact]
        public void DeclareFloat()
        {
            var obj = new LispFloat(1.5);
            Assert.True(LispKernel.TryGetDeclarationStatementForObject(obj, "X", out var declaration));
            Assert.Equal("(SETF X 1.5) ()", declaration);
        }

        [Fact]
        public void DeclareInteger()
        {
            var obj = new LispInteger(1);
            Assert.True(LispKernel.TryGetDeclarationStatementForObject(obj, "X", out var declaration));
            Assert.Equal("(SETF X 1) ()", declaration);
        }

        [Fact]
        public void DeclareRatio()
        {
            var obj = new LispRatio(1, 2);
            Assert.True(LispKernel.TryGetDeclarationStatementForObject(obj, "X", out var declaration));
            Assert.Equal("(SETF X 1/2) ()", declaration);
        }

        [Fact]
        public void DeclareComplexNumber()
        {
            var obj = new LispComplexNumber(new LispInteger(1), new LispInteger(2));
            Assert.True(LispKernel.TryGetDeclarationStatementForObject(obj, "X", out var declaration));
            Assert.Equal("(SETF X #C(1 2)) ()", declaration);
        }

        [Fact]
        public void DeclareCharacter()
        {
            var obj = new LispCharacter('a');
            Assert.True(LispKernel.TryGetDeclarationStatementForObject(obj, "X", out var declaration));
            Assert.Equal(@"(SETF X #\a) ()", declaration);
        }

        [Fact]
        public void DeclareString()
        {
            var obj = new LispString("ab\"c");
            Assert.True(LispKernel.TryGetDeclarationStatementForObject(obj, "X", out var declaration));
            Assert.Equal(@"(SETF X ""ab\""c"") ()", declaration);
        }

        [Fact]
        public void DeclareList()
        {
            var obj = LispList.FromItems(new LispInteger(1), new LispInteger(2));
            Assert.True(LispKernel.TryGetDeclarationStatementForObject(obj, "X", out var declaration));
            Assert.Equal("(SETF X '(1 2)) ()", declaration);
        }
    }
}
