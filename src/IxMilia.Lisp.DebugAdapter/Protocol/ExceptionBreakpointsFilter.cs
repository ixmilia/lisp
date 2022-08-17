public class ExceptionBreakpointsFilter
{
    public string Filter { get; set; }
    public string Label { get; set; }

    public ExceptionBreakpointsFilter()
    {
        Filter = "error";
        Label = "Error breakpoint";
    }
}
