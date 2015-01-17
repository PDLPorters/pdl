
package PDL::Graphics::VRML::Protos;
PDL::Graphics::VRMLNode->import();
PDL::Graphics::VRMLProto->import();

sub PDLBlockText10 {
	vrp('PDLBlockText10',
		[fv3f('position',"0 0 0"),
		 fv3f('size',"1 1 0.1"),
		 fmstr('text','["TESTING PDL","TEXTBLOCK","LONG LONG LONG LONG LONG TEXT","Short","FOOOOOOOOOOOOOOOO"]')],
		vrn('Transform',
		    'translation' => 'IS position',
		    'scale' => 'IS size',
		    'children' => [
		        vrn(Transform,
			    translation => '0 0 -0.55',
			    'children' => [vrn('Shape',
			    	  geometry => vrn('Box', size => '1 1 0.45'),
				  appearance => vrn(Appearance,
					material => vrn(Material,
						   diffuseColor => '0.9 0.9 0.9',
						   ambientIntensity => '0.1'
					)
				  )
			       )]
			   ),
		        vrn(Transform,
			    translation => '-0.45 0.35 0',
			    scale => '0.9 0.9 0',
			    children => [
				vrn(Shape,
				  geometry => vrn(Text,
						    string => 'IS text',
						    maxExtent => '1.0',
						    fontStyle => 
						       vrn(FontStyle,
							   size => '0.075',
							   spacing => '1.33',
							   justify => 'end'
							 ),
						  ),
				  appearance => vrn(Appearance,
					material => vrn(Material,
						   diffuseColor => '0 0 0',
						   ambientIntensity => '0'
					)
				  )
				)
			])
		    ]
		 )
	);
}

1;
