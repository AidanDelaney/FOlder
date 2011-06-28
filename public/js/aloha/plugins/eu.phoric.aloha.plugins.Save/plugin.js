if(typeof EXAMPLE=="undefined"||!EXAMPLE)
{
    var EXAMPLE={}
}

EXAMPLE.DummySavePlugin=new GENTICS.Aloha.Plugin("eu.phoric.aloha.plugins.Save");
EXAMPLE.DummySavePlugin.languages=["en"];

EXAMPLE.DummySavePlugin.init=function(){
    var that=this;
    var saveButton=new GENTICS.Aloha.ui.Button({label:this.i18n("save"),onclick:function(){that.save()}});
    
    GENTICS.Aloha.FloatingMenu.addButton('GENTICS.Aloha.global', saveButton, 'Format', 0);
};

EXAMPLE.DummySavePlugin.save=function(){
    var content= encodeURIComponent($('div#foldr').html());

    $.ajax({
	type : "POST",
	url  : "",
        data : 'content=' + content + '&title="Folder"',
	//success: function(msg){}; // do nothing
    });
};