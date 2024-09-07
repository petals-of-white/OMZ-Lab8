#version 330 core

out vec4 FragColor;
in vec2 TexCoord;
uniform sampler2D tex1;

void main()
{
    vec3 color = texture(tex1, TexCoord).rrr;
    FragColor = vec4(color,1);
}