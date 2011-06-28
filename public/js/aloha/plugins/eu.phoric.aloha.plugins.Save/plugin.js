if(typeof EXAMPLE=="undefined"||!EXAMPLE)
{
  var EXAMPLE={}
}

EXAMPLE.DummySavePlugin=new GENTICS.Aloha.Plugin("eu.phoric.aloha.plugins.Save");
EXAMPLE.DummySavePlugin.languages=["en"];

EXAMPLE.DummySavePlugin.init=function(){
  var that=this;
  var saveButton=new GENTICS.Aloha.ui.Button({label:this.i18n("save"),onclick:function(){that.save()}});

    GENTICS.Aloha.FloatingMenu.addButton('GENTICS.Aloha.global', saveButton, 'Format', 0)};
  EXAMPLE.DummySavePlugin.save=function(){
   var content="";
   jQuery.each(GENTICS.Aloha.editables,function(index,editable){content=content+"Editable ID: "+editable.getId()+"\nHTML code: "+editable.getContents()+"\n\n"});
   alert("<div>"+content+"</div>")
  };