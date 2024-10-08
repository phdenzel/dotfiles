precision mediump float;
varying vec2 v_texcoord;
uniform sampler2D tex;

void main() {

    vec4 pixColor = texture2D(tex, v_texcoord);

    // red
    pixColor[0] *= 0.6;
    // green
    pixColor[1] *= 0.6;
    // blue
    pixColor[2] *= 0.6;

    gl_FragColor = pixColor;
}
