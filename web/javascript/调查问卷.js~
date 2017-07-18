$(function(){
    $(".content input[name^='radio']").click(function(){    //input[name='radio'] 单选按钮只要被点击且不管你点它多少次  他的选中状态都是true        
        $(this).parent("li").addClass("checked").siblings("li").removeClass("checked").parents(".content").attr("data-id","checkBox");               

        var contentLen = $(".content").length;
        var checkLen = $("div[data-id='checkBox']").length;

        checked(contentLen,checkLen);
    });

    $(".content_checkbox").each(function(){
        var self = $(this);
        $(this).find("input[name^='checkbox']").click(function(){
            if(this.checked == true){            
                $(this).parent("li").addClass("checked");               
            }else{
                $(this).parent("li").removeClass("checked");
            }

            if(self.find("li").hasClass("checked")){
                self.attr("data-id","checkBox");
            }else{
                self.removeAttr("data-id");
            }

            var contentLen = $(".content").length;
            var checkLen = $("div[data-id='checkBox']").length;

            checked(contentLen,checkLen);
        });

        
    });

function checked(contentLen,checkLen){
    if(contentLen == checkLen){
       $("#submit").css({"background":"#3b7ded"});
    }else{
       $("#submit").css({"background":"#9f9f9f"});
    }
}
});