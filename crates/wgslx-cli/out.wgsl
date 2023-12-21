struct LightInner {
    test: vec3<f32>,
}

struct Light {
    test: vec3<f32>,
    inner: vec3<i32>,
}

const other_exported2_: vec3<f32> = vec3<f32>(1.0, 1.0, 1.0);
const third_exported: vec3<f32> = vec3(1.0);
const other_exported: vec3<f32> = vec3(1.0);

fn make_light(value: vec3<f32>, inner: vec3<i32>) -> Light {
    return Light(value, inner);
}

fn make_light2_(value_1: vec3<f32>, inner_1: vec3<i32>) -> Light {
    return Light(value_1, inner_1);
}

fn calculate_tbn_mat3x3_(value_2: Light, value2_: Light) -> mat3x3<f32> {
    var nebula: vec3<f32> = third_exported;

    let g = vec3(1.0);
    let _e9 = make_light(vec3(1.0), vec3(1));
    return mat3x3<f32>(g, _e9.test, value_2.test);
}

