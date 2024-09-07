#version 330 core

out vec4 FragColor;
in vec2 TexCoord;
uniform sampler2D tex1;
uniform sampler2D tex2;

void main()
{
    float r = texture(tex1, TexCoord).r;
    float g = texture(tex2, TexCoord).r;
    FragColor = vec4(r,g,0,1);
}