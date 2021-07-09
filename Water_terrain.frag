#version 460 core

subroutine vec4 shadeIsland();//two stages shading, for terrain and water

//Input
in VerticesGS{
	vec4 position_world;
	vec4 position_clip;
	vec2 texture_coor;

} fs_in;
in LightingGS{
	vec3 viewDir_tangent;
	vec3 sunDir_tangent;
	vec4 position_light_clip;//light space position

} fs_lit_in;
//Output
layout (location = 0) out vec4 FragColor;

//uniforms
uniform float tex_horizontalscale;//enlarge the texture coordinate to shrink the texture and thus more details

uniform float waveStrength;//control the strength of the DUDV map
uniform float moveFactor;//control how the wave is moved, [0,1]
uniform float normalStrength;//control how strong the water normal is, since it controls the z value of the normal map so the latger the weaker.
uniform vec3 cameraPos;//The world position of the camera
uniform float shadowSize;

//textures
layout (binding = 1) uniform sampler2DArray terrainTex;//multitexturing
//contains water reflection and refraction and the entire terrain scene
//We only operate the first 2 element to render water, the last texture is binded to framebuffer for drawing
layout (binding = 2) uniform sampler2DArray waterRef;
layout (binding = 3) uniform sampler2DArray waterEffect;//water dudv map and normal
layout (binding = 4) uniform sampler2D terrainNormal;//calculated normal map for the terrain
layout (binding = 5) uniform sampler2DArray terrainSplatting;//calculated splat map of the terrain, rgba16
layout (binding = 6) uniform sampler2DArray TerrainDepth;//depth map of the three rendering output
layout (binding = 7) uniform sampler2DShadow TerrainShadow;

layout (location = 0) subroutine uniform shadeIsland shader;

const float REFLECTION_AMOUT = 0.5f;
const float NEAR = 0.3f;
const float FAR = 300.0f;
const vec4 FOG_COLOR = vec4(0.9f, 0.9f, 0.9f, 1.0f);

//functions
vec4 fog(vec4);
vec4 calcLight(vec3, vec4, float);
float calcShadow();

layout (index = 0) subroutine(shadeIsland) vec4 shadeTerrain(){//render 2 water ref. textures and the terrain without water
	const vec2 scaled_texcoord = fs_in.texture_coor * tex_horizontalscale;
	vec4 terrainCol;
	float splatFactor[6];//we have 6 splat factors in total
	vec4 splatTex;
	//load up the splat factor from texture array 0
	splatTex = texture(terrainSplatting, vec3(fs_in.texture_coor, 0.0f));
	splatFactor[0] = splatTex.r;
	splatFactor[1] = splatTex.g;
	splatFactor[2] = splatTex.b;
	splatFactor[3] = splatTex.a;
	//then array 1
	splatTex = texture(terrainSplatting, vec3(fs_in.texture_coor, 1.0f));
	splatFactor[4] = splatTex.r;
	splatFactor[5] = splatTex.g;//rest of the channels are unused
	
	//mixing...mixing...
	for(int i = 0; i < 5; i++){
		terrainCol += texture(terrainTex, vec3(scaled_texcoord, i)) * splatFactor[i];
	}
	//the slop factor is independent (sum with other channel will make the result greater than 1) so we need to mix it
	terrainCol = mix(terrainCol, texture(terrainTex, vec3(scaled_texcoord, 5.0f)), splatFactor[5]);
	terrainCol.a = 1.0f;
	
	//calcultate fresnel reflection/refraction factor
	vec3 normal = texture(terrainNormal, fs_in.texture_coor).rgb * 2.0f - 1.0f;
	normal = normalize(normal);

	return calcLight(normal, terrainCol, 16.0f);
}

layout (index = 1) subroutine(shadeIsland) vec4 shadeWater(){//render the water
	const vec2 scaled_texcoord = fs_in.texture_coor * tex_horizontalscale;
	//we need to sample the normalised device coordinate
	vec2 ndc = (fs_in.position_clip.xy / fs_in.position_clip.w) / 2.0f + 0.5f;//and convert to [0,1], making origin at (0,0) (was [-1,1])
	vec2 reflecTexcoor = vec2(ndc.x, 1.0f - ndc.y);//flip the reflection texture over
	vec2 refracTexcoor = vec2(ndc.x, ndc.y);

	//water depth calculations
	//get the depth map from camera to the floor of the water
	float depth = texture(TerrainDepth, vec3(refracTexcoor, 1.0f)).r;//we only need depth below the water
	float floorDistance = 2.0f * NEAR * FAR / (FAR + NEAR - (2.0f * depth - 1.0f) * (FAR - NEAR));//distance to floor
	//distance to the water surface
	depth = gl_FragCoord.z;
	float waterDistance = 2.0f * NEAR * FAR / (FAR + NEAR - (2.0f * depth - 1.0f) * (FAR - NEAR));
	float waterDepth = floorDistance - waterDistance;

	//sample the DUDV map and mess around with the texture coordinate
	//we want our wave to be moving in different direction so we sample two DUDV and move them separatly
	vec2 distortion1 = texture(waterEffect, vec3(scaled_texcoord.x + moveFactor, scaled_texcoord.y, 0.0f)).rg;
	vec2 distortion2 = texture(waterEffect, vec3(scaled_texcoord.x, scaled_texcoord.y + moveFactor, 0.0f)).rg;
	//convert to [-1,1], and since the texture coordinate is changing we want the edge of the water to be smoother
	vec2 distortion = ((distortion1 + distortion2) * 2.0f - 1.0f) * waveStrength * clamp(waterDepth / 15.0f, 0.0f, 1.0f);

	reflecTexcoor += distortion;
	reflecTexcoor = clamp(reflecTexcoor, 0.001f, 0.999f);//clamp the range so the reflection and refraction will not be warp around the border
	//-----------------------------------------------------------
	refracTexcoor += distortion;
	refracTexcoor = clamp(refracTexcoor, 0.001f, 0.999f);

	//calculate ref texture
	vec4 reflecCol = texture(waterRef, vec3(reflecTexcoor, 0.0f));
	vec4 refracCol = texture(waterRef, vec3(refracTexcoor, 1.0f));

	//calcultate fresnel reflection/refraction factor
	vec3 normal = texture(waterEffect, vec3(distortion, 1.0f)).rgb * 2.0f - 1.0f;

	normal = normalize(normal * vec3(1.0f, 1.0f, normalStrength));//making the normal map more "blue" will decrease the strength
	float refractiveFactor = dot(normalize(fs_lit_in.viewDir_tangent), normal);//viewdir has been normalised
	refractiveFactor = pow(refractiveFactor, REFLECTION_AMOUT);//the higher the power, the more reflective the water is

	//specular lighting for water
	vec3 halfwayDir = normalize(fs_lit_in.sunDir_tangent + fs_lit_in.viewDir_tangent);
	float specularFactor = pow(max(dot(normal, halfwayDir), 0.0f), 64.0f) * clamp(waterDepth / 5.0f, 0.0f, 1.0f);

	//return color
	vec4 waterCol = mix(reflecCol, refracCol, smoothstep(0.0f, 1.95f, refractiveFactor));
	waterCol = mix(waterCol, vec4(0.0f, 0.3f, 0.5f, 1.0f), 0.2f) + vec4(specularFactor);//make our water looks a bit "blue"
	waterCol.a = clamp(waterDepth / 5.0f, 0.0f, 1.0f);//when we approch the edge of the water, we want the water to look smoother

	return waterCol;
} 

void main(){
	FragColor = fog(shader());
}

vec4 fog(vec4 inColor){
	float viewDistance = distance(cameraPos, fs_in.position_world.xyz);
	float visibility = exp(-pow(viewDistance * 0.0034f, 4.2f));
	visibility = clamp(visibility, 0.0f, 1.0f);

	return mix(FOG_COLOR, inColor, visibility);
}

vec4 calcLight(vec3 normal, vec4 diffCol, float specPow){
	const vec3 lightCol = vec3(251.0f / 255.0f, 254.0f / 255.0f, 188.0f / 255.0f);
	//ambient
	vec3 ambient = lightCol * 0.22f;
	//diffuse
	//by conventions, light direction is from fragment to light and now the sundir starts from the light
	//parallel light, all fragments have the same light direction, has been normalized
	vec3 diffuse = lightCol * max(dot(-fs_lit_in.sunDir_tangent, normal), 0.0f) * 0.6f * calcShadow();
	//specular
	vec3 halfwayDir = normalize(fs_lit_in.sunDir_tangent + fs_lit_in.viewDir_tangent);
	vec3 specular = lightCol * pow(max(dot(normal, halfwayDir), 0.0f), specPow) * 0.95f * calcShadow();

	return diffCol * vec4(ambient + diffuse + specular, 1.0f);
}

float calcShadow(){
	float visibility = 0.0f;
	int size = 0;

	//A modified version of PCF
	for(int i = -2; i <= 2; i++){
		for(int j = -2; j <= 2; j++){

			visibility += textureProj(TerrainShadow, vec4(fs_lit_in.position_light_clip.xy + vec2(i, j) / shadowSize, fs_lit_in.position_light_clip.zw), 0.005);
			size++;
		}
	}
	//sampling around the texture coordinates and get the average
	visibility /= size * 1.0f;

	return visibility;
	
}