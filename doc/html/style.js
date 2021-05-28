//-*-Mode:javascript;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=javascript fenc=utf-8 sts=4 ts=4 sw=4 et nomod:

function clipboard_copy(text)
{
    var clipboard_text = document.getElementById('clipboard_text');
    if (clipboard_text === null)
    {
        clipboard_text = document.createElement('textarea');
        clipboard_text.id = 'clipboard_text';
        clipboard_text.style.border = '0';
        clipboard_text.style.padding = '0';
        clipboard_text.style.margin = '0';
        clipboard_text.style.position = 'absolute';
        clipboard_text.style.fontSize = '12pt'; // prevent iOS zooming

        // keep the element off-screen
        if (document.documentElement.getAttribute('dir') == 'rtl')
            clipboard_text.style.right = '-9999px';
        else
            clipboard_text.style.left = '-9999px';

        // prevent mobile devices from opening a keyboard
        clipboard_text.setAttribute('readonly', true);

        document.body.append(clipboard_text);
    }
    // move the textarea to the same vertical position
    if (window.pageYOffset)
        clipboard_text.style.top = window.pageYOffset + 'px';
    else
        clipboard_text.style.top = document.documentElement.scrollTop + 'px';
    // set the textarea text for a clipboard copy
    clipboard_text.value = text;
    clipboard_text.focus();
    clipboard_text.select();

    var copied = false;
    try
    {
        // clipboard copy requires:
        // Internet Explorer 10+
        // Google Chrome 43+ (~April 2015)
        // Mozilla Firefox 41+ (shipping ~September 2015)
        // Opera 29+ (based on Chromium 42, ~April 2015)
        if (document.execCommand('copy'))
            copied = true;
    }
    catch (err)
    {
        console.error('copy to clipboard failed: ', err);
    }
    return copied;
}

function clipboard_copy_click(button, text)
{
    return function(){
        if (clipboard_copy(text))
        {
            button.innerHTML = button.getAttribute('html_action');
            window.setTimeout(function() {
                button.innerHTML = button.getAttribute('html_content');
            }, 2000);
        }
    }
}

function clipboard_copy_init()
{
    var button_class_name = 'clipboard_copy';
    var button_html_content = '&nbsp;COPY&nbsp;';
    var button_html_action = 'COPIED';
    // fonts lack unicode support for a decent "copy" character
    //var button_html_content = '&#x2398;'; // copy (NEXT PAGE) character
    //var button_html_action = '&#x1f4cb;'; // CLIPBOARD character
    var element_array = document.getElementsByClassName('code');
    for (var i = 0; i < element_array.length; i++)
    {
        var element = element_array[i];
        var button = document.createElement('button');
        button.className = button_class_name;
        button.innerHTML = button_html_content;
        button.setAttribute('html_content', button_html_content);
        button.setAttribute('html_action', button_html_action);
        button.onclick = clipboard_copy_click(button, element.textContent);
        element.appendChild(button);
    }
}

