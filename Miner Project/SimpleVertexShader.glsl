#version 330 core
layout(location=0) in vec4 vertexPosition_modelspace;
layout(location=1) in vec2 vertexUV;
layout(location=2) in vec3 vertexNormal_modelspace;

out vec2 uv;

uniform mat4 MVP;

void main() {
	gl_Position = MVP * vertexPosition_modelspace;
	uv = vertexUV;
}