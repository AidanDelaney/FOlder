(function(window,undefined){var jQuery=window.alohaQuery||window.jQuery,$=jQuery,GENTICS=window.GENTICS,Aloha=window.Aloha;Aloha.DragAndDropFiles=new (Aloha.Plugin.extend({_constructor:function(){this._super("dragndropfiles")},languages:["en","fr"],config:{drop:{max_file_size:300000,max_file_count:2,upload:{uploader_instance:"Aloha.Repositories.Uploader",config:{callback:function(resp){return resp},method:"POST",url:"",accept:"application/json",file_name_param:"filename",file_name_header:"X-File-Name",extra_headers:{},extra_post_data:{},send_multipart_form:false,image:{max_width:800,max_height:800},www_encoded:false}}}},init:function(){var that=this;Aloha.loadJs(Aloha.getPluginUrl("dragndropfiles")+"/src/dropfilesrepository.js",function(){that.setBodyDropHandler();if(that.settings===undefined){that.settings=that.config}else{that.settings=jQuery.extend(true,that.config,that.settings)}try{that.uploader=that.initUploader(that.settings)}catch(error){Aloha.Log.warn(that,error);Aloha.Log.warn(that,"Error creating uploader, no upload will be processed")}});Aloha.bind("aloha-file-upload-prepared",function(event,data){if(that.droppedFilesCount>=that.processedFiles){Aloha.trigger("aloha-allfiles-upload-prepared")}});Aloha.bind("aloha-allfiles-upload-prepared",function(event,data){var len=that.filesObjs.length;if(that.dropInEditable){Aloha.trigger("aloha-drop-files-in-editable",{filesObjs:that.filesObjs,range:that.targetRange,editable:that.targetEditable});var edConfig=that.getEditableConfig(that.targetEditable);while(--len>=0){that.uploader.startFileUpload(that.filesObjs[len].id,edConfig.drop.upload.config)}}else{Aloha.trigger("aloha-drop-files-in-page",that.filesObjs);while(--len>=0){that.uploader.startFileUpload(that.filesObjs[len].id,that.config.drop.upload.config)}}})},initUploader:function(customConfig){var uploader_instance;try{uploader_instance=eval(customConfig.drop.upload.uploader_instance)}catch(error){Aloha.Log.info(this,"Custom class loading error or not specified, using default");uploader_instance=Aloha.Repositories.Uploader}return uploader_instance},prepareFileUpload:function(file){var reader=new FileReader(),fileObj,that=this;reader.file=file;reader.onloadend=function(){var currentFile={name:this.file.name,type:this.file.type,fileSize:this.file.fileSize,fileName:this.file.fileName,data:reader.result};that.filesObjs.push(that.uploader.addFileUpload(currentFile));that.processedFiles++;Aloha.trigger("aloha-file-upload-prepared",fileObj)};reader.readAsDataURL(file)},dropEventHandler:function(event){var that=this,edConfig,len,target,files=event.dataTransfer.files;this.targetEditable=undefined;this.droppedFilesCount=files.length;this.processedFiles=0;Aloha.Log.info(that,this.droppedFilesCount+" files have been dropped on the page");if(!event.dataTransfer&&!event.dataTransfer.files){event.sink=false;return true}if(this.droppedFilesCount<1){event.sink=false;return true}if(event.preventDefault){event.preventDefault()}else{event.cancelBubble=true}if(this.droppedFilesCount>that.settings.drop.max_file_count){Aloha.Log.warn(that,"too much files dropped");if(event.stopPropagation){event.stopPropagation()}else{event.returnValue=false}return true}target=jQuery(event.target);if(target.hasClass("aloha-editable")){this.targetEditable=target;target=this.targetEditable.children(":last");if(target.hasClass("aloha-editable")){this.targetEditable.append("<span> </span>");target=this.targetEditable.children(":last")}}else{this.targetEditable=target.parents(".aloha-editable")}this.filesObjs=[];this.dropInEditable=false;len=this.droppedFilesCount;if(this.targetEditable[0]===null){while(--len>=0){if(!(!!document.createElement("canvas").getContext&&files[len].type.match(/image\//)&&edConfig.drop.upload.config.image)){if(files[len].size<=that.settings.drop.max_file_size){that.prepareFileUpload(files[len])}else{this.processedFiles++;Aloha.Log.warn(that,"max_file_size exeeded, upload of "+files[len].name+" aborted")}}else{that.prepareFileUpload(files[len])}}}else{Aloha.getEditableById(this.targetEditable.attr("id")).activate();that.targetRange=that.initializeRangeForDropEvent(event,this.targetEditable);edConfig=that.getEditableConfig(this.targetEditable);if(edConfig.drop){that.dropInEditable=true}while(--len>=0){if(!(!!document.createElement("canvas").getContext&&files[len].type.match(/image\//)&&edConfig.drop.upload.config.image)){if(files[len].size<=edConfig.drop.max_file_size){that.prepareFileUpload(files[len])}else{this.processedFiles++;Aloha.Log.warn(that,"max_file_size exeeded, upload of "+files[len].name+" aborted")}}else{that.prepareFileUpload(files[len])}}}if(event.stopPropagation){event.stopPropagation()}else{event.returnValue=false}return false},setBodyDropHandler:function(){var that=this;if(!document.body.BodyDragSinker){document.body.BodyDragSinker=true;this.onstr="";this.mydoc=document;this.methodName="addEventListener";if(jQuery.browser.msie){this.onstr="on";this.methodName="attachEvent";this.mydoc=document.body}this.mydoc[this.methodName](this.onstr+"drop",function(event){that.dropEventHandler(event)},false);this.mydoc[this.methodName](this.onstr+"dragenter",function(event){if(event.preventDefault){event.preventDefault()}else{event.cancelBubble=true}if(event.stopPropagation){event.stopPropagation()}else{event.returnValue=false}return false},false);this.mydoc[this.methodName](this.onstr+"dragleave",function(event){if(event.preventDefault){event.preventDefault()}else{event.cancelBubble=true}if(event.stopPropagation){event.stopPropagation()}else{event.returnValue=false}return false},false);this.mydoc[this.methodName](this.onstr+"dragover",function(event){if(event.preventDefault){event.preventDefault()}else{event.cancelBubble=true}if(event.stopPropagation){event.stopPropagation()}else{event.returnValue=false}},false)}},initializeRangeForDropEvent:function(event,editable){var target=jQuery(event.target);var range=new Aloha.Selection.SelectionRange(true);range.update();if(target.textNodes().length==0){range.startContainer=target[0].childNodes[0];range.endContainer=target[0].childNodes[0]}else{range.startContainer=target.textNodes()[0];range.endContainer=target.textNodes()[0]}range.startOffset=0;range.endOffset=0;try{range.select()}catch(error){Aloha.Log.error(this,error)}return range}}))()})(window,document);