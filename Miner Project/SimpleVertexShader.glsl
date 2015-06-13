#version 330 core
layout(location=0) in vec3 vertexPosition_modelspace;
layout(location=1) in vec2 vertexUV;

out vec2 uv;

uniform mat4 MVP;

void main() {
	vec4 v = vec4(vertexPosition_modelspace, 1.0);
	gl_Position = MVP * v;

	uv = vertexUV;
}