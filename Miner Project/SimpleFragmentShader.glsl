#version 330 core

in vec2 uv;
out vec3 colour;

uniform sampler2D myTextureSampler;


void main() {
	colour = texture( myTextureSampler, uv ).rgb;
}
