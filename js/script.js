
var botones = document.getElementsByClassName('button0');

function asignarColor(Color){ 
   document.getElementById('colores').style.background = Color

   function inputColor() {
    var color = document.getElementById('color_generator').value;
    
    document.getElementById('colores').style.background = color;
  
   document.getElementById('valor_hex').style.display = "flex";
   document.getElementById('valor_hex').value = color;
  }

  document.getElementById('color_generator').addEventListener('input', inputColor);
}

asignarColor()

for (i=0; i<botones.length; i++) {
    botones[i].onclick = function(){asignarColor(this.innerHTML)}
};

//Colores1

var botones1 = document.getElementsByClassName('button1');

function asignarColor1 (Color1){

   document.getElementById('colores1').style.background = Color1;
  
   function inputColor() {
    var color1 = document.getElementById('color_generator1').value;
  
    document.getElementById('colores1').style.background = color1;
    

    document.getElementById('valor_hex1').style.display = "flex";
    document.getElementById('valor_hex1').value = color1;
  }
  
  document.getElementById('color_generator1').addEventListener('input', inputColor); 
};

asignarColor1();

for (i=0; i<botones1.length; i++) {
    botones1[i].onclick = function(){asignarColor1(this.innerHTML)}
};

//Colores2

var botones2 = document.getElementsByClassName('button2');

function asignarColor2(Color2){
    document.getElementById('colores2').style.background = Color2;

    function inputColor() {
      var color2 = document.getElementById('color_generator2').value;
    
      document.getElementById('colores2').style.background = color2;
    
      document.getElementById('valor_hex2').style.display = "flex";
      document.getElementById('valor_hex2').value = color2;
    }
    
    document.getElementById('color_generator2').addEventListener('input', inputColor);
};

asignarColor2();

for (i=0; i<botones2.length; i++) {
    botones2[i].onclick = function(){asignarColor2(this.innerHTML)}
};

//Colores3
var botones3 = document.getElementsByClassName('button3');

function asignarColor3 (Color3){
   document.getElementById('colores3').style.background = Color3;

   function inputColor() {
    var color3 = document.getElementById('color_generator3').value;
  
    document.getElementById('colores3').style.background = color3;
  
    document.getElementById('valor_hex3').style.display = "flex";
    document.getElementById('valor_hex3').value = color3;
  }
  
  document.getElementById('color_generator3').addEventListener('input', inputColor);
};

asignarColor3();

for (i=0; i<botones3.length; i++) {
    botones3[i].onclick = function(){asignarColor3(this.innerHTML)}
};

//Colores4

var botones4 = document.getElementsByClassName('button4');

function asignarColor4(Color4){ 
   document.getElementById('colores4').style.background = Color4;

   function inputColor() {
    var color4 = document.getElementById('color_generator4').value;
  
    document.getElementById('colores4').style.background = color4;
  
    document.getElementById('valor_hex4').style.display = "flex";
    document.getElementById('valor_hex4').value = color4;
  }
  
  document.getElementById('color_generator4').addEventListener('input', inputColor); 
};

asignarColor4();

for (i=0; i<botones4.length; i++) {
    botones4[i].onclick = function(){asignarColor4(this.innerHTML)}   
};