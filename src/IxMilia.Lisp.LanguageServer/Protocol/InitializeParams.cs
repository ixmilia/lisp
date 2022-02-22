using System.Collections.Generic;

namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class InitializeParams
    {
        public int ProcessId { get; set; }
        public List<WorkspaceFolder> WorkspaceFolders { get; } = new List<WorkspaceFolder>();

        public InitializeParams(int processId, IEnumerable<WorkspaceFolder> workspaceFolders)
        {
            ProcessId = processId;
            if (workspaceFolders is object)
            {
                WorkspaceFolders.AddRange(workspaceFolders);
            }
        }
    }
}
