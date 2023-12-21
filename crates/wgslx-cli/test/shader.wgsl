
@import "other.wgsl"

const other_exported2 = vec3<f32>(1., 1., 1.,); //vec3f(

struct LightInner {
  test: vec3f,
};

struct Light {
  test: vec3f,
  inner: vec3i,
};


fn make_light(value: vec3f, inner: vec3i) -> Light {
  return Light( value, inner ); 
}

fn make_light2(value: vec3f, inner: vec3i) -> Light {
  return Light( value, inner ); 
}

alias FooMat3 = mat3x3f;


fn calculate_tbn_mat3x3(value: Light, value2: Light) -> FooMat3 {
  // let g = vec3(1.);
  // let ng = vec3(1.);
  var nebula = third_exported; 
  // var nebula2 = other; 
  // var n = value2.test;
  // var t = tangent;
  // var g3 = item_sum; 
  // Leyngel's calculation (Foundations of Game Engine Development, p. 122),
  // but why would we need to do it this way? Does result in a very slightly
  // different visual
  // var t = normalize(tangent - n * dot(tangent, n));
  // var b = normalize(cross(normal, tangent) * tangent_sigma);
  // let g = vec3<f32>(1.); 
  let g = vec3(1.); 
  let g2 = make_light( vec3(1.), vec3(1) );

  return FooMat3(g, g2.test, value.test); 
}


// // Vertex shader
// struct VertexInput {
//   @builtin(instance_index) instance : u32,
 
//   @location(0) position: vec3<f32>,
//   @location(1) normal: vec3<f32>,
//   @location(2) texcoords0: vec2<f32>,
//   @location(3) tangent: vec4<f32>,
// };

// struct VertexOutput {
//   @builtin(position) clip_position: vec4<f32>,
//   @location(0) pos: vec3<f32>, 
//   @location(1) color: vec3<f32>,
//   @location(2) normal: vec3<f32>, 
//   @location(3) texcoords0: vec2<f32>, 
//   @location(4) tangent: vec3<f32>, 
//   @location(5) @interpolate(flat) tangent_sigma: f32, 
//   @location(6) @interpolate(flat) instance: u32, 
// };

// struct Camera {
//  projection: mat4x4<f32>,
//  view: mat4x4<f32>,
//  post: vec3f
// }; 

// @group(0) @binding(0) var<storage> transforms: array<mat4x4<f32>>;
// @group(0) @binding(1) var<storage> normal_transforms: array<mat4x4<f32>>;
// @group(0) @binding(2) var color_texture: texture_2d<f32>;
// @group(0) @binding(3) var color_sampler: sampler; 
// @group(0) @binding(4) var normal_texture: texture_2d<f32>;

// @group(1) @binding(0) var<uniform> camera: Camera;

// const PI = 3.14; 

// struct Light {
//  pos: vec3<f32>,
//  color: vec3<f32>
// };

// struct Material {
//  base_color: vec3<f32>,
//  roughness: f32,
// }

// fn foobar(a: f32) -> f32 {
//   return a; 
// }

// /**
//  * Calculate the tangent-bitangent-normal transform, needed to convert from tangent-space
//  * to the cooridnate space of the normal and tanget (here, view-space). As it is scale only,
//  * we return as a vec3
//  */
// fn calculate_tbn_mat3x3(normal: vec3<f32>, tangent: vec3<f32>, tangent_sigma: f32) -> mat3x3<f32> {
//   // var n = normal;
//   var n = other_exported; 
//   var t = tangent;
//   // Leyngel's calculation (Foundations of Game Engine Development, p. 122),
//   // but why would we need to do it this way? Does result in a very slightly
//   // different visual
//   // var t = normalize(tangent - n * dot(tangent, n));
//   var b = normalize(cross(normal, tangent) * tangent_sigma);

//   return mat3x3(t, b, n); 
// }


// @vertex
// fn vs_main(model: VertexInput) -> VertexOutput {
//   var out: VertexOutput;
//   var pos = camera.view * transforms[model.instance] * vec4<f32>(model.position, 1.0);

//   out.normal = (normal_transforms[model.instance] * vec4<f32>(model.normal, 0.)).xyz; 
//   // out.normal = vec4<f32>(model.normal, 0.).xyz;  
//   out.pos = pos.xyz; 
//   out.clip_position = camera.projection * pos;
//   out.texcoords0 = model.texcoords0; 

//   // Unpack tangent vector, defined by GLTF spec.
//   var unpacked_sigma = model.tangent * 2. - 1.; 
//   out.tangent = (camera.view * transforms[model.instance] * vec4<f32>(unpacked_sigma.xyz, 0.0)).xyz;
//   out.tangent_sigma = unpacked_sigma.w; 
  
//   out.color = vec3(.25); // (normal + 1.) / 2.; //vec3<f32>(1., 0., 0.); //model.color;

//   return out;
// }

// fn light_intensity_get(light: Light, pos_to_light: vec3<f32>) -> vec3<f32> {
//   // For now, assume directional light only
//   // Otherwise, should have a squared falloff based on the distance to the light
//   return light.color; 
// }


// @fragment
// fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {

//   var indirect_color_intensity = .25;
  
//   var light0 = Light
//     (// position, view space
//      (camera.view * vec4<f32>(0., 10., -10., 1.)).xyz,
//      // color
//      vec3<f32>(1.0, 1.0, 1.0));

//   var light1 = Light
//     (// position, view space
//      (camera.view * vec4<f32>(10., 0., 0., 1.)).xyz,
//      // color
//      vec3<f32>(1.0, 1.0, 1.0) * .75);

//   var light2 = Light
//     (// position, view space
//      (camera.view * vec4<f32>(0., -5., 10., 1.)).xyz,
//      // color
//      vec3<f32>(1.0, 1.0, 1.0) * .4);

//   var lights = array(light0, light1, light2);

//   // var material = Material
//   //   (// base_color
//   //    vec3(1., 0., 0.), 
//   //    // diffuse_color
//   //    vec3<f32>(.5, 0., 0.),
//   //    // specular_color
//   //    vec3<f32>(.75));

//   var color = textureSample(color_texture, color_sampler, in.texcoords0);
//   var material = Material
//     (// base_color / albedo
//      color.xyz, 
//      // roughness
//      .4
//      );
 

//   var in_normal = normalize(in.normal);
//   var in_tangent = normalize(in.tangent);
//   // in_tangent.z *= -1.; 
  
//   var in_tangent_sigma = in.tangent_sigma; 
//   var tbn_mat3x3 = calculate_tbn_mat3x3(in_normal, in_tangent, in_tangent_sigma);

//   // Unpack normal
//   var normal_color = textureSample(normal_texture, color_sampler, in.texcoords0).xyz * 2. - 1.;

//   // Flip to left-hand (gltf designed for WebGL/OpenGL coordinate space)
//   normal_color.y *= -1.; 
  
//   var normal_tangent_space = normalize(normal_color); 
//   var normal = normalize(tbn_mat3x3 * normal_tangent_space);
//   // var normal = in_normal; 

//   if (true) {
//     // return vec4<f32>(normal.xyz, 1.); 
//     // return vec4<f32>(normal_tangent_space.xyz, 1.); 
//     // return vec4<f32>(in_tangent.xyz, 1.);
//   }

//   var direct_color = vec3<f32>(0.); 

//   for (var i = 0; i < 3; i++) {
//     var light = lights[i];

//     var pos_to_light = normalize(light.pos - in.pos);
//     var pos_to_eye = normalize(-in.pos); // Eye is at origin in view-space
//     var halfway = normalize(pos_to_eye + pos_to_light);
//     var is_behind = f32(dot(normal, pos_to_light) > 0.);



//     // Rendering equation for point lights:
//     //
//     // L_0 (V) = PI * LightIntensity * NdotL * (BRDF_diffuse + BRDF_specular)

//     // PI comes from the derivation of solving a point light source light 
//     // direct_color += PI * intensity * NdotL * lambertian_diffuse_brdf(material);
//     // direct_color += PI * intensity * NdotL * is_behind * specular_brdf(material, normal, halfway);

//     var brdf = lambertian_diffuse_brdf(material) + is_behind * specular_brdf(material, normal, halfway);
//     var intensity = light_intensity_get(light, pos_to_light);
//     var NdotL = max(dot(normal, pos_to_light), 0.);

//     direct_color += PI * intensity * NdotL * brdf; 
//   }

//   // Use simple ambient color for indirect
//   var indirect_color = material.base_color * indirect_color_intensity; 

//   return vec4<f32>(indirect_color + direct_color.xyz, 1.0);
// }


// // BRDF Equation:
// //
// //                      Active surface points
// //                     --------------
// //              F(L,H) G(L,V,H) NDC(H)
// // BRDF(L,V) = -------------------------
// //                4(NdotL)(NdotV) <- Correction factor
// //
// //  where
// //    NDC(H) is the microgeometry normal distribution fn evaluated at the halfway (H) vector,
// //      the concentration of surface points which *could* reflect light from L to V
// //    G(L,V,H) is the geometry function, the % of surface points not shadowed or masked at H.
// //      Combined with NDC, determines the active surface points that participate in reflecting
// //      light from L into V
// //    F(L,H) is the Fresnel reflectance of active surface points, detailing how much of the incoming
// //      light is reflected from each surface point
// //    4(NdotL)(NdotV) is the correction factor, accounting for quantities being transformed from
// //      microgeometry space to the overall macrosurface
// //
// // Often the geometry and correction term are combined into a single "visibility term", yielding:
// //
// // BRDF_specular(L,V) = (1/4) * F(L, H) * Visibility(L, V, H) * NDC(H)
// //

// fn lambertian_diffuse_brdf(material: Material) -> vec3<f32> {
//   // The lambertian BRDF is constant.
//   // The cosine (Normal_dot_PosToLight) is part of the reflection equation. 
//   // https://seblagarde.wordpress.com/2012/01/08/pi-or-not-to-pi-in-game-lighting-equation/
//   return material.base_color / PI; 
// }

// // Normal Distribution Function (NDF)
// // 
// // Microgeometry is not uniform in surface orientation, with more normals pointing "up" than
// // "sidways" (away from n). The NDF determines the statistical distribution of surface orientations.
// // 
// //  - Value can be arbitrarily large & is a scalar
// //  - In microfacet BRDF terms, NDF is evaluated for the direction h to determine the the concentration
// //    of active surface points (those for which m = h) 

// // NDF_phong(M) = intensity + 2         intensity
// //                ------------- (NdotM)^
// //                    2PI
// //   where
// //     M = microfacent normal
// fn NDF_phong(material: Material,
//              normal: vec3<f32>,
//              halfway: vec3<f32>) -> f32 {

//   var intensity = pow(1000., 1. - material.roughness);
//   var normalization = (intensity + 2. /** approx. between 2-4 */) / (2. * PI);
//   var NdotH = dot(normal, halfway); 
//   var highlight = pow(max(NdotH, 0.), intensity);

//   return highlight * normalization; 
// }

// fn Fresnel_phong(material: Material) -> vec3<f32> {
//   // To be energy conserving, we need to make sure the diffuse & specular colors are <= 1
//   // Since we don't expose a specular color param, derive it from the base_color
//   return (1. - material.base_color) * material.base_color; 
// }

// fn Visibility_phong() -> f32 {
//   return 1.; 
// }

// // Instead of intensity, should be "roughness"
// fn specular_brdf(material: Material,
//                  normal: vec3<f32>,
//                  halfway: vec3<f32>) -> vec3<f32> {

//   return Fresnel_phong(material) * Visibility_phong() * NDF_phong(material, normal, halfway) * 0.25; 
// }


