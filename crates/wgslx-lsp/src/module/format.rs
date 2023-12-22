use naga::{StorageAccess, Interpolation};

fn format_scalar(scalar: naga::Scalar) -> String {
  let bits = scalar.width * 8;
  
  match scalar.kind {
    naga::ScalarKind::Sint => format!("i{}", bits),
    naga::ScalarKind::Uint => format!("u{}", bits),
    naga::ScalarKind::Float => format!("f{}", bits),
    naga::ScalarKind::Bool => "bool".to_string(),
    naga::ScalarKind::AbstractInt => format!("abstract i{}", bits),
    naga::ScalarKind::AbstractFloat => format!("abstract f{}", bits),
  }
}

fn format_address_space(space: naga::AddressSpace) -> String {
  match space {
    naga::AddressSpace::Function => "function".to_string(),
    naga::AddressSpace::Private => "private".to_string(),
    naga::AddressSpace::WorkGroup => "workgroup".to_string(),
    naga::AddressSpace::Uniform => "uniform".to_string(),
    naga::AddressSpace::Storage { access } => match access {
      StorageAccess::LOAD =>  "storage<read>".to_string(),
      StorageAccess::STORE =>  "storage<write>".to_string(),
      _ =>  "storage<read_write>".to_string(),
    },
    naga::AddressSpace::Handle => "handle".to_string(),
    naga::AddressSpace::PushConstant => "push_constant".to_string(),
  }
}

fn format_vector(size: naga::VectorSize, scalar: naga::Scalar) -> String {
  let scalar_str = format_scalar(scalar); 

  match size {
    naga::VectorSize::Bi => format!("vec2<{}>", scalar_str),
    naga::VectorSize::Tri => format!("vec3<{}>", scalar_str),
    naga::VectorSize::Quad => format!("vec4<{}>", scalar_str),
  }
}

pub fn format_function_definition(module: &naga::Module, func: naga::Handle<naga::Function>) -> String {
  let func = &module.functions[func]; 
  let ty_str = if let Some(result) = &func.result {
    let ty = &module.types[result.ty];

    match ty.inner {
      naga::TypeInner::Struct { .. } => ty.name.clone().unwrap(),
      _ => format_type(module, ty)
    }
  } else {
    "void".to_string()
  };

  let arg_str = func.arguments.iter()
    .map(|arg| format_bindable(module, &arg.name, arg.ty, &arg.binding))
    .collect::<Vec<_>>()
    .join(", "); 

  format!("fn {}({arg_str}): {ty_str}", func.name.clone().unwrap())
}

fn format_bindable(module: &naga::Module, name: &Option<String>, ty_handle: naga::Handle<naga::Type>, binding: &Option<naga::Binding>) -> String {
  let name_str = name.clone().unwrap();
  let ty = &module.types[ty_handle];
  let ty_str = match &ty.inner {
    // Avoid printing entire struct
    naga::TypeInner::Struct { .. } => ty.name.clone().unwrap(),
    _ => format_type(module, &module.types[ty_handle])
  };
  
  match binding {
    None => format!("{}: {}", name_str, ty_str),
    Some(naga::Binding::BuiltIn(_)) => format!("@builtin {}: {}", name_str, ty_str),
    Some(naga::Binding::Location { location, interpolation, sampling, .. }) => {
      let mut prefix = format!("@location({})", location);
    
      if let Some(interpolation) = interpolation {
        prefix += match interpolation {
          Interpolation::Perspective => " perspective",
          Interpolation::Linear => " linear",
          Interpolation::Flat => " flat",
        };
      }

      if let Some(sampling) = sampling {
        prefix += match sampling {
            naga::Sampling::Center => " center",
            naga::Sampling::Centroid => " centroid",
            naga::Sampling::Sample => " sample",
        };
      }; 

      format!("{} {}: {}", prefix, name_str, ty_str)
    }
  }
}

pub fn format_type(module: &naga::Module, ty: &naga::Type) -> String {
  match &ty.inner {
    naga::TypeInner::Scalar(scalar) => format_scalar(*scalar),
    naga::TypeInner::Vector { size, scalar } => format_vector(*size, *scalar),
    naga::TypeInner::Matrix { columns, rows, scalar } => {
      let scalar_str = format_scalar(*scalar);

      match (columns, rows) {
        (naga::VectorSize::Bi, naga::VectorSize::Bi) => format!("mat2x2<{}>", scalar_str),
        (naga::VectorSize::Bi, naga::VectorSize::Tri) => format!("mat2x3<{}>", scalar_str),
        (naga::VectorSize::Bi, naga::VectorSize::Quad) => format!("mat2x4<{}>", scalar_str),
        (naga::VectorSize::Tri, naga::VectorSize::Bi) => format!("mat3x2<{}>", scalar_str),
        (naga::VectorSize::Tri, naga::VectorSize::Tri) => format!("mat3x3<{}>", scalar_str),
        (naga::VectorSize::Tri, naga::VectorSize::Quad) => format!("mat3x4<{}>", scalar_str),
        (naga::VectorSize::Quad, naga::VectorSize::Bi) => format!("mat4x2<{}>", scalar_str),
        (naga::VectorSize::Quad, naga::VectorSize::Tri) => format!("mat4x3<{}>", scalar_str),
        (naga::VectorSize::Quad, naga::VectorSize::Quad) => format!("mat4x4<{}>", scalar_str),
      }
    },
    naga::TypeInner::Atomic(scalar) => format!("atomic<{}>", format_scalar(*scalar)),
    naga::TypeInner::Pointer { base, space } => {
      let space_str = format_address_space(*space); 
      let base_str = format_type(module, &module.types[*base]);

      format!("ptr<{}, {}>", space_str, base_str)
    },
    naga::TypeInner::ValuePointer { size, scalar, space } => {
      let space_str = format_address_space(*space); 

      if let Some(size) = size {
        let vec_str = format_vector(*size, *scalar);

        return format!("ptr<{}, {}>", space_str, vec_str)
      }

      let scalar_str = format_scalar(*scalar);

      format!("ptr<{}, {}>", space_str, scalar_str)
    }
    naga::TypeInner::Array { base, size, .. } => {
      let base_str = format_type(module, &module.types[*base]);

      match size {
        naga::ArraySize::Constant(size) => format!("array<{}, {}>", base_str, size),
        naga::ArraySize::Dynamic => format!("array<{}>", base_str)
    }
    },
    naga::TypeInner::Struct { members, .. } => {
      let name_str = ty.name.clone().unwrap();
      let members_str = members.iter().map(|member| format_bindable(module, &member.name, member.ty, &member.binding)).collect::<Vec<_>>().join(", ");

      format!("struct {} {{ {} }}", name_str, members_str)
    },
    naga::TypeInner::Image { dim, arrayed, class } => {
      let mut array_str = String::new();

      if *arrayed {
        array_str += "_array"; 
      }

      match class {
        naga::ImageClass::Sampled { kind, multi } => {
          let scalar_str = match kind {
            naga::ScalarKind::Sint => "i32",
            naga::ScalarKind::Uint => "u32",
            naga::ScalarKind::Float => "f32",
            naga::ScalarKind::Bool => "bool",
            naga::ScalarKind::AbstractInt => "abstract int",
            naga::ScalarKind::AbstractFloat => "abstract float",
          }; 
          
          let multi_str = String::new();

          if *multi {
            array_str += "_multisampled"; 
          }

          match dim {
            naga::ImageDimension::D1 => format!("texture{}_1d{}<{}>", multi_str, array_str, scalar_str),
            naga::ImageDimension::D2 => format!("texture{}_2d{}<{}>", multi_str, array_str, scalar_str),
            naga::ImageDimension::D3 => format!("texture{}_3d{}<{}>", multi_str, array_str, scalar_str),
            naga::ImageDimension::Cube => format!("texture{}_cube{}<{}>", multi_str, array_str, scalar_str),
          }
        },
        naga::ImageClass::Depth { multi } => {
          let multi_str = String::new();

          if *multi {
            array_str += "_multisampled"; 
          }

          match dim {
            naga::ImageDimension::D1 => format!("texture_depth{}_1d{}", multi_str, array_str),
            naga::ImageDimension::D2 => format!("texture_depth{}_2d{}", multi_str, array_str),
            naga::ImageDimension::D3 => format!("texture_depth{}_3d{}", multi_str, array_str),
            naga::ImageDimension::Cube => format!("texture_depth{}_cube{}", multi_str, array_str),
          }
        },
        naga::ImageClass::Storage { .. } => match dim {
          naga::ImageDimension::D1 => format!("texture_storage_1d{}", array_str),
          naga::ImageDimension::D2 => format!("texture_storage_2d{}", array_str),
          naga::ImageDimension::D3 => format!("texture_storage_3d{}", array_str),
          naga::ImageDimension::Cube => format!("texture_storage_cube{}", array_str),
        },
      }
    },
    naga::TypeInner::Sampler { .. } => "sampler".to_string(),
    naga::TypeInner::AccelerationStructure => "acceleration structure".to_string(),
    naga::TypeInner::RayQuery => "ray query".to_string(), 
    naga::TypeInner::BindingArray { base, size } => {
      let base_str = format_type(module, &module.types[*base]);

      match size {
        naga::ArraySize::Constant(size) => format!("binding_array<{}, {}>", base_str, size),
        naga::ArraySize::Dynamic => format!("binding_array<{}>", base_str),
      }
    },
  }
}
