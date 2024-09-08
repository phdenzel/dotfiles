precision mediump float;
varying vec2 v_texcoord;
uniform sampler2D tex;

void main() {

    vec4 pixColor = texture2D(tex, v_texcoord);

    // red
    pixColor[0] *= 0.9;
    // green
    pixColor[1] *= 0.9;
    // blue
    pixColor[2] *= 0.9;

    gl_FragColor = pixColor;
}
