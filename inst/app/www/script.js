$('#Turistas').children('div').children('div').children('div').children('div').addClass('seleccionado');

let id = "Turistas"

$('#Turistas, #Empleo, #Programas').on('click', function (event) {
	event.stopPropagation();
	$('.infopop').css('visibility', 'hidden')
	$('.seleccionado').removeClass('seleccionado');
	id = $(this).attr('id');
	$(this).children('div').children('div').children('div').children('div').addClass('seleccionado');
});

$('#Turistas,  #Empleo, #Programas').on("mouseenter", function (event) {
	event.stopPropagation();
	if (id != $(this).attr('id')) {
		console.log("laksdjflasjdflñasjdfñlasj");
		$(this).children('div').children('div').children('div').children('div').addClass('hover');
	};
}).on("mouseleave", function (event) {
	event.stopPropagation();
	$(this).children('div').children('div').children('div').children('div').removeClass('hover');
});

// $('#infoBarras').on('click', function(){
// 	if ($('.infopop').css('visibility') == 'visible') {
// 		$('.infopop').css('visibility', 'hidden')
// 	} else {
// 		$('.infopop').css('visibility', 'visible')
// 	}
// })

// $('#infoInstalaciones').on('click', function(){
// 	if ($('.infopopInstalaciones').css('visibility') == 'visible') {
// 		$('.infopopInstalaciones').css('visibility', 'hidden')
// 	} else {
// 		$('.infopopInstalaciones').css('visibility', 'visible')
// 	}
// })

// var descripcionesBarras = new Object();
// descripcionesBarras.Turistas = "<p>-Descripción: Número de turistas deportivos en el municipio en el último año</p><p>-Fuente: Ayuntamiento de ______</p><p>-Obtención: Obtención directa</p>";
// descripcionesBarras.Eventos = "<p>-Descripción: Número de eventos deportivos en el municipio en el último año</p><p>-Fuente: Ayuntamiento de ______</p><p>-Obtención: Obtención directa</p>";
// descripcionesBarras.Alojamientos = "<p>-Descripción: Ocupación de los alojamientos del municipio en el último año</p><p>-Fuente: Establecimientos hoteleros</p><p>-Obtención: Cociente entre habitaciones ocupadas y total de habitaciones</p>";
// descripcionesBarras.Transporte = "<p>-Descripción: Número de personas que han utilizado el transporte público del municipio</p><p>-Fuente: Ayuntamiento de ______</p><p>-Obtención: Obtención directa</p>";

// const infoBarras = document.querySelector('.infoContent');
// const infoBarrasInstalaciones = document.querySelector('.infoContentInstalaciones');
// infoBarras.innerHTML = descripcionesBarras.Turistas;
// infoBarrasInstalaciones.innerHTML = "<p>-Descripción: Número de instalaciones en el municipio</p><p>-Fuente: Ayuntamiento de ______</p><p>-Obtención: Obtención directa</p>";

// const botonInfoBarras = document.querySelector('#infoBarras');
// const botonInfoInstalaciones = document.querySelector('#infoInstalaciones');

// document.addEventListener('click', (e) => {
//   if (!botonInfoBarras.contains(e.target) && !infoBarras.contains(e.target)) {
// 	$('.infopop').css('visibility', 'hidden');
//   }
//   if (!botonInfoInstalaciones.contains(e.target) && !infoBarrasInstalaciones.contains(e.target)) {
// 	$('.infopopInstalaciones').css('visibility', 'hidden');
//   }
// });

$('#indicadores').addClass('under');
$('#indicadores, #instalaciones, #evento, #clubes, #economico, #metodologia, #ambiental, #social').on('click',function(){
	// $('.menuSelec').removeClass('menuSelec');
	$('.under').removeClass('under');
	// $(this).addClass('menuSelec');
	$(this).addClass('under');
});

// $('#graficoClub').on('click', function() {
// 	if ($(this)[0].innerHTML == "Ver miembros") {
// 		$(this)[0].innerHTML = "Ver clubes";
// 	} else {
// 		$(this)[0].innerHTML = "Ver miembros";
// 	}
// })

function setZoom() {
	if (window.matchMedia('(height <= 850px) and (width <= 1150px)').matches) {
		document.querySelector("#fooSocial > div.box-body > div.col-sm-4 > div > div").style.transform = "scale(0.75)";
	}  else if (document.querySelector("#fooSocial > div.box-body > div.col-sm-4 > div > div")){
		document.querySelector("#fooSocial > div.box-body > div.col-sm-4 > div > div").style.transform = "scale(1)";
	}
	
}

setZoom();
window.addEventListener('resize', setZoom);