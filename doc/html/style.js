//-*-Mode:javascript;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=javascript fenc=utf-8 sts=4 ts=4 sw=4 et nomod:

function element_select(element)
{
    if (window.getSelection && document.createRange)
    {
        // IE 9 and non-IE
        var range = document.createRange();
        range.selectNodeContents(element);
        var selection = window.getSelection();
        selection.removeAllRanges();
        selection.addRange(range);
    }
    else if (document.body.createTextRange)
    {
        // IE < 9
        var textRange = document.body.createTextRange();
        textRange.moveToElementText(element);
        textRange.select();
    }
    try
    {
        // copy requires:
        // Internet Explorer 10+
        // Google Chrome 43+ (~April 2015)
        // Mozilla Firefox 41+ (shipping ~September 2015)
        // Opera 29+ (based on Chromium 42, ~April 2015)
        document.execCommand('copy');
    }
    catch (err)
    {
    }
}
