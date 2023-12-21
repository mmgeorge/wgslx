
struct VertexInput {
    @builtin(instance_index) instance: u32,
    @location(0) position: vec3<f32>,
    @location(1) normal: vec3<f32>,
    @location(2) texcoords0_: vec2<f32>,
    @location(3) tangent: vec4<f32>,
}

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) pos: vec3<f32>,
    @location(1) color: vec3<f32>,
    @location(2) normal: vec3<f32>,
    @location(3) texcoords0_: vec2<f32>,
    @location(4) tangent: vec3<f32>,
    @location(5) @interpolate(flat) tangent_sigma: f32,
    @location(6) @interpolate(flat) instance: u32,
}

struct Camera {
    projection: mat4x4<f32>,
    view: mat4x4<f32>,
    post: vec3<f32>,
}

struct Light {
    pos: vec3<f32>,
    color: vec3<f32>,
}

struct Material {
    base_color: vec3<f32>,
    roughness: f32,
}

const other_exported: vec3<f32> = vec3(1.0);
const PI: f32 = 3.14;

@group(0) @binding(0) 
var<storage> transforms: array<mat4x4<f32>>;
@group(0) @binding(1) 
var<storage> normal_transforms: array<mat4x4<f32>>;
@group(0) @binding(2) 
var color_texture: texture_2d<f32>;
@group(0) @binding(3) 
var color_sampler: sampler;
@group(0) @binding(4) 
var normal_texture: texture_2d<f32>;
@group(1) @binding(0) 
var<uniform> camera: Camera;

fn foobar(a: f32) -> f32 {
    return a;
}

fn calculate_tbn_mat3x3_(normal_1: vec3<f32>, tangent: vec3<f32>, tangent_sigma: f32) -> mat3x3<f32> {
    var n: vec3<f32> = other_exported;
    var t: vec3<f32>;
    var b: vec3<f32>;

    t = tangent;
    b = normalize((cross(normal_1, tangent) * tangent_sigma));
    let _e10 = t;
    let _e11 = b;
    let _e12 = n;
    return mat3x3<f32>(_e10, _e11, _e12);
}

fn light_intensity_get(light_1: Light, pos_to_light_1: vec3<f32>) -> vec3<f32> {
    return light_1.color;
}

fn lambertian_diffuse_brdf(material_1: Material) -> vec3<f32> {
    return (material_1.base_color / vec3(3.14));
}

fn Fresnel_phong(material_2: Material) -> vec3<f32> {
    return ((vec3(1.0) - material_2.base_color) * material_2.base_color);
}

fn Visibility_phong() -> f32 {
    return 1.0;
}

fn NDF_phong(material_3: Material, normal_2: vec3<f32>, halfway_1: vec3<f32>) -> f32 {
    var intensity_1: f32;
    var normalization: f32;
    var NdotH: f32;
    var highlight: f32;

    intensity_1 = pow(1000.0, (1.0 - material_3.roughness));
    let _e9 = intensity_1;
    normalization = ((_e9 + 2.0) / 6.28);
    NdotH = dot(normal_2, halfway_1);
    let _e17 = NdotH;
    let _e20 = intensity_1;
    highlight = pow(max(_e17, 0.0), _e20);
    let _e23 = highlight;
    let _e24 = normalization;
    return (_e23 * _e24);
}

fn specular_brdf(material_4: Material, normal_3: vec3<f32>, halfway_2: vec3<f32>) -> vec3<f32> {
    let _e3 = Fresnel_phong(material_4);
    let _e4 = Visibility_phong();
    let _e6 = NDF_phong(material_4, normal_3, halfway_2);
    return (((_e3 * _e4) * _e6) * 0.25);
}

@vertex 
fn vs_main(model: VertexInput) -> VertexOutput {
    var out: VertexOutput;
    var pos: vec4<f32>;
    var unpacked_sigma: vec4<f32>;

    let _e4 = camera.view;
    let _e8 = transforms[model.instance];
    pos = ((_e4 * _e8) * vec4<f32>(model.position, 1.0));
    let _e19 = normal_transforms[model.instance];
    out.normal = (_e19 * vec4<f32>(model.normal, 0.0)).xyz;
    let _e26 = pos;
    out.pos = _e26.xyz;
    let _e31 = camera.projection;
    let _e32 = pos;
    out.clip_position = (_e31 * _e32);
    out.texcoords0_ = model.texcoords0_;
    unpacked_sigma = ((model.tangent * 2.0) - vec4(1.0));
    let _e46 = camera.view;
    let _e50 = transforms[model.instance];
    let _e52 = unpacked_sigma;
    out.tangent = ((_e46 * _e50) * vec4<f32>(_e52.xyz, 0.0)).xyz;
    let _e60 = unpacked_sigma.w;
    out.tangent_sigma = _e60;
    out.color = vec3(0.25);
    let _e64 = out;
    return _e64;
}

@fragment 
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    var indirect_color_intensity: f32 = 0.25;
    var light0_: Light;
    var light1_: Light;
    var light2_: Light;
    var lights: array<Light, 3>;
    var color: vec4<f32>;
    var material: Material;
    var in_normal: vec3<f32>;
    var in_tangent: vec3<f32>;
    var in_tangent_sigma: f32;
    var tbn_mat3x3_: mat3x3<f32>;
    var normal_color: vec3<f32>;
    var normal_tangent_space: vec3<f32>;
    var normal: vec3<f32>;
    var direct_color: vec3<f32> = vec3(0.0);
    var i: i32 = 0;
    var light: Light;
    var pos_to_light: vec3<f32>;
    var pos_to_eye: vec3<f32>;
    var halfway: vec3<f32>;
    var is_behind: f32;
    var brdf: vec3<f32>;
    var intensity: vec3<f32>;
    var NdotL: f32;
    var indirect_color: vec3<f32>;

    let _e5 = camera.view;
    light0_ = Light((_e5 * vec4<f32>(0.0, 10.0, -10.0, 1.0)).xyz, vec3<f32>(1.0, 1.0, 1.0));
    let _e21 = camera.view;
    light1_ = Light((_e21 * vec4<f32>(10.0, 0.0, 0.0, 1.0)).xyz, vec3<f32>(0.75, 0.75, 0.75));
    let _e37 = camera.view;
    light2_ = Light((_e37 * vec4<f32>(0.0, -5.0, 10.0, 1.0)).xyz, vec3<f32>(0.4, 0.4, 0.4));
    let _e51 = light0_;
    let _e52 = light1_;
    let _e53 = light2_;
    lights = array<Light, 3>(_e51, _e52, _e53);
    let _e59 = textureSample(color_texture, color_sampler, in.texcoords0_);
    color = _e59;
    let _e61 = color;
    material = Material(_e61.xyz, 0.4);
    in_normal = normalize(in.normal);
    in_tangent = normalize(in.tangent);
    in_tangent_sigma = in.tangent_sigma;
    let _e74 = in_normal;
    let _e75 = in_tangent;
    let _e76 = in_tangent_sigma;
    let _e77 = calculate_tbn_mat3x3_(_e74, _e75, _e76);
    tbn_mat3x3_ = _e77;
    let _e82 = textureSample(normal_texture, color_sampler, in.texcoords0_);
    normal_color = ((_e82.xyz * 2.0) - vec3(1.0));
    let _e92 = normal_color.y;
    normal_color.y = (_e92 * -1.0);
    let _e94 = normal_color;
    normal_tangent_space = normalize(_e94);
    let _e97 = tbn_mat3x3_;
    let _e98 = normal_tangent_space;
    normal = normalize((_e97 * _e98));
    if true {
    }
    loop {
        let _e108 = i;
        if (_e108 < 3) {
        } else {
            break;
        }
        {
            let _e111 = i;
            let _e113 = lights[_e111];
            light = _e113;
            let _e116 = light.pos;
            pos_to_light = normalize((_e116 - in.pos));
            pos_to_eye = normalize(-(in.pos));
            let _e125 = pos_to_eye;
            let _e126 = pos_to_light;
            halfway = normalize((_e125 + _e126));
            let _e130 = normal;
            let _e131 = pos_to_light;
            is_behind = f32((dot(_e130, _e131) > 0.0));
            let _e137 = material;
            let _e138 = lambertian_diffuse_brdf(_e137);
            let _e139 = is_behind;
            let _e140 = material;
            let _e141 = normal;
            let _e142 = halfway;
            let _e143 = specular_brdf(_e140, _e141, _e142);
            brdf = (_e138 + (_e139 * _e143));
            let _e147 = light;
            let _e148 = pos_to_light;
            let _e149 = light_intensity_get(_e147, _e148);
            intensity = _e149;
            let _e151 = normal;
            let _e152 = pos_to_light;
            NdotL = max(dot(_e151, _e152), 0.0);
            let _e158 = intensity;
            let _e160 = NdotL;
            let _e162 = brdf;
            let _e164 = direct_color;
            direct_color = (_e164 + (((PI * _e158) * _e160) * _e162));
        }
        continuing {
            let _e167 = i;
            i = (_e167 + 1);
        }
    }
    let _e170 = material.base_color;
    let _e171 = indirect_color_intensity;
    indirect_color = (_e170 * _e171);
    let _e174 = indirect_color;
    let _e175 = direct_color;
    return vec4<f32>((_e174 + _e175.xyz), 1.0);
}
