Proyecciones de la Idoneidad de Hábitat de Echinometra lucunter en el Caribe

Este repositorio contiene los datos, scripts y resultados del estudio sobre el impacto del cambio climático en el erizo de roca Echinometra lucunter en el Caribe suroccidental (Colombia, Panamá, Costa Rica y Nicaragua).
📌 Descripción del Proyecto

El estudio evalúa cómo la acidificación oceánica (pH) y el aumento de la temperatura superficial del mar afectarán la distribución de E. lucunter para el periodo 2040–2050, utilizando un enfoque de Ensamble de Modelos de Nicho Ecológico (ENM).
🛠️ Metodología

Se integraron cuatro algoritmos de aprendizaje automático y estadística:

    MaxEnt (Máxima Entropía)

    GLM (Modelos Lineales Generalizados)

    GAM (Modelos Aditivos Generalizados)

    Random Forest (Bosque Aleatorio)

Variables Oceanográficas

Se utilizaron capas de MARSPEC y Bio-ORACLE v3.0, incluyendo:

    Físicas: Batimetría, Distancia a la costa.

    Químicas: Rango de pH, Rango de Temperatura, Clorofila-a.

📂 Estructura del Repositorio

    /data: Ocurrencias depuradas (GBIF) y registros de pseudo-ausencias.

    /scripts: Código en R para la limpieza de datos, validación cruzada (k=5) y ensamble.

    /results: Mapas de idoneidad en formato .TIFF e importancia de variables.

✍️ Autores

    Ronaldo D. Campo R. - Investigador Principal - rcamporomero88@correo.unicordoba.edu.co

    Andrés F. Diaz D. - Co-autor - andresdiazd@correo.unicordoba.edu.co

🎓 Instituciones

    Universidad de Córdoba, Colombia.

    Laboratorios La Ciénaga.

⚖️ Licencia

Este proyecto está bajo la Licencia Creative Commons Atribución 4.0 Internacional (CC BY 4.0).

Usted es libre de:

    Compartir: Copiar y redistribuir el material en cualquier medio o formato.

    Adaptar: Remezclar, transformar y construir a partir del material para cualquier propósito, incluso comercialmente.

Bajo los siguientes términos:

    Atribución: Debe dar crédito de manera adecuada, brindar un enlace a la licencia e indicar si se han realizado cambios.

📄 Citación

Si utilizas estos datos o scripts en tu investigación, por favor cita de la siguiente manera:

    Campo, R. D. & Diaz, A. F. (2026). Proyecciones de la idoneidad de hábitat de Echinometra lucunter en el Caribe, bajo escenarios de cambio climático. Universidad de Córdoba, Colombia.

Contacto: Para dudas sobre la implementación del modelo o acceso a las capas raster procesadas, contactar a los autores.
