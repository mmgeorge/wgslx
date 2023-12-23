

// pub fn codegen(module: &naga::Module) -> String {
  
// }

// EntryPoint::vs_main

use std::marker::PhantomData;


// -- lib
// Use naga instead

mod ty {
  pub struct Vec3f; 
  pub struct Vec4f; 
}

struct Dimension1D;
struct Dimension2D;

struct RGBAF32; 

enum Visibility {
  Vertex, 
  Fragment
}

struct Location<TWgslType> {
  binding: u32,
  ty: TWgslType
}

struct TextureDesc<TDimmension, TFormat> {
  tag: PhantomData<(TDimmension, TFormat)>,
  binding: u32,
  visibility: Visibility, 
}

struct SamplerDesc {}


// Dimension2D::GetDim
// RGBAF32:GetType

struct Bindgroup0 {
  sprite: TextureDesc<Dimension2D, RGBAF32>,
  sprite_sampler: SamplerDesc, 
}

impl Bindgroup0 {
  fn get() -> Self {
    Self {
      sprite: TextureDesc {
        tag: PhantomData, 
        binding: 0,
        visibility: Visibility::Vertex
      },
      sprite_sampler: SamplerDesc {}
    }
  }
}

struct FooVsMainLocations {
  position: Location<ty::Vec3f>, 
  color: Location<ty::Vec4f>, 
}

struct FooShaderModule {
  pub vs_main: FooVsMainLocations, 
  pub vs_frag: FooVsMainLocations
}


trait ShaderModule {}
trait VertexEntryPoint {}
trait FragmentEntryPoint {}


struct VertexState<TEntry: VertexEntryPoint> {
  entry: TEntry
}

struct FragmentState<TEntry: FragmentEntryPoint> {
  entry: TEntry
}

struct PipelineDesc<TShader: ShaderModule, TVertex: VertexEntryPoint, TFragment: FragmentEntryPoint> {
  shader: VertexState<TVertex>,
  vertex: VertexState<TVertex>,
  fragment: FragmentState<TFragment>,
}

struct Pipeline<TShaderModule, TVertexEntry, TFragmentEntry> {
  tag: PhantomData<(TShaderModule, TVertexEntry, TFragmentEntry)> 
}


struct Layout {
  l0: u32, 
}

fn do_main() {
  let bg0 = Bindgroup0::get();

  let pipeline = fooShaderModule.createPipeline(FooShaderModulePipelineDesc {
    vertex: &FragmentState<..> {
      entry: fooShaderModule.vertex.vs_main,
      attributes: &VsMainAttributes {
        position: {
          buffer: myBuffer,
          offset: 0
        }
        other: buffer1.attribute[0]
      }
      // buffers: [] // attributes in all buffers must add up to attribs in entry.. how to type this?
      // specify a list of buffers, and then this is just pointing to that?
      // 
    }, 
    fragment: &FragmentState<..> {
      entry: fooShaderModule.fragment.fs_main,
    }, 
    layout: &Layout {
      bind_groups_layouts: (  // <- Typed from module type
        &shader::BindGroupLayout0::default(), 
        &shader::BindGroupLayout1::default(), 
      )
    },
    primitive: {} ,
    depth_stencil: {}
  });

  fooShaderModule.create_bind_group();
  
  
  let x = FooPipelineDesc {
    shader, 
    vertex: bg0.sprite,
    fragment: bg0.sprite_sampler
  };

  let pipeline = PipelineBuilder::new(x)
    .setLayout(PipelineBuilder::PipelineLayout {
      
    })
}



// -- codegen

enum EntryPoint {
  vs_main(Entry_vs_main), 
}


struct ShaderModule {
  
}


enum VertexInput {
  Position {
    location: 0,
    ty: vec3f
  },
  
}

