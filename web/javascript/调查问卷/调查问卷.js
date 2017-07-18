//document.writeln("hahha")
//计算成绩的方法
function calcute_score() {
	//for(i=1;i<=10;i++)
	//document.writeln(i);
	var score=0;
	var rank;
	var zk=document.getElementsByName("radio1");
	var qj=document.getElementsByName("radio2");
	var xh=document.getElementsByName("radio3");
	var dj=document.getElementsByName("checkbox0");
	//for(var i=0;i<zk.length;i++)
	//document.writeln(zk[i].value);
	//score+=5;
	if(zk[0].checked)
	score+=5;
	if(qj[0].checked)
	score+=5;
	if(xh[0].checked)
	score+=5;
	if(dj[0].checked)
	score+=5;
	//alert(score);
	switch(score) {
		case 0:rank="你是人渣";break;
		case 5:rank="行吧";break;
		case 10:rank="好人";break;
		case 15:rank="你是好人";break;
		case 20:rank="你没救了";break;
		default:rank="计分出错了";break;
	}
	//alert(rank);
	return score;
}

function save() {
	var score=calcute_score();
	alert("你最终的得分是"+score+"\n");
	//var score=calcute_score();
	var name=document.getElementById("fname");
	var email=document.getElementById("email");
	localStorage.setItem(name,score,email);
}
//进行验证的方法
function validate()
{
var at=document.getElementById("email").value.indexOf("@")
var fname=document.getElementById("fname").value
submitOK="true"

if (fname.length>10)
 {
 alert("名字必须小于 10 个字符。")
 submitOK="false"
 }
if (at==-1) 
 {
 alert("不是有效的电子邮件地址。")
 submitOK="false"
 }
if (submitOK=="false")
 {
 return false
 }
 if(submitOK=="true")
 {
 	save();//存储在本地的文件中
 }
}

	var tFlag = 0;  
    var tPass = 0;
	function timer(id) {  
        if (tFlag != 0) {  
            var tNew = new Date().getTime();  
            tPass = tPass + (tNew - tFlag);  
 			tFlag = tNew;  

        } else {  
            tFlag = new Date().getTime();  
        }  
        setTimeout("timer('" + id + "')", 100);   
        var sc = Math.floor((tPass / 1000) % 60);  
        var mi = Math.floor((tPass / 1000 / 60) % 60);  
        var hr = Math.floor((tPass / 1000/ 60 / 60) % 24);  
        var info =hr + "时" + mi + "分" + sc + "秒";  
        document.getElementById(id).innerHTML = info;  
    }



















