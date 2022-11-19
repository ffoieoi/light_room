Shader "SupGames/SSAOURP"
{
    Properties
    {
        _MainTex ("Texture", 2D) = "white" {}
    }
    SubShader
    {
		HLSLINCLUDE
		#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"

		TEXTURE2D_X_FLOAT(_CameraDepthTexture);
		SAMPLER(sampler_CameraDepthTexture);

		TEXTURE2D_X(_BlurTex);
		SAMPLER(sampler_BlurTex);

		TEXTURE2D_X(_MainTex);
		SAMPLER(sampler_MainTex);

		#define SAMPLE_DEPTH(uv) LinearEyeDepth(SAMPLE_DEPTH_TEXTURE(_CameraDepthTexture, sampler_CameraDepthTexture, uv).r, _ZBufferParams);

		half4 _CameraDepthTexture_TexelSize;
		half4 _MainTex_TexelSize;
		half _Intensity;
		half _Radius;
		half _BlurAmount;
		half _Area;

		struct appdata
		{
			half4 pos : POSITION;
			half2 uv : TEXCOORD0;
			UNITY_VERTEX_INPUT_INSTANCE_ID
		};

		struct v2f {
			half4 pos : SV_POSITION;
			half2 uv  : TEXCOORD0;
			UNITY_VERTEX_INPUT_INSTANCE_ID
			UNITY_VERTEX_OUTPUT_STEREO
		};

		struct v2fb
		{
			half4 pos  : SV_POSITION;
			half2  uv  : TEXCOORD0;
			half4  uv1  : TEXCOORD1;
			UNITY_VERTEX_INPUT_INSTANCE_ID
			UNITY_VERTEX_OUTPUT_STEREO
		};
		struct v2fs
		{
			float4 pos  : SV_POSITION;
			float4  uv  : TEXCOORD0;
			float4  uv1  : TEXCOORD1;
			float3  uv2  : TEXCOORD2;
			UNITY_VERTEX_INPUT_INSTANCE_ID
			UNITY_VERTEX_OUTPUT_STEREO
		};

		v2f vert(appdata i)
		{
			v2f o = (v2f)0;
			UNITY_SETUP_INSTANCE_ID(i);
			UNITY_TRANSFER_INSTANCE_ID(i, o);
			UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);
			o.pos = mul(unity_MatrixVP, mul(unity_ObjectToWorld, half4(i.pos.xyz, 1.0h)));
			o.uv = UnityStereoTransformScreenSpaceTex(i.uv);
			return o;
		}

		v2fb vertb(appdata i)
		{
			v2fb o = (v2fb)0;
			UNITY_SETUP_INSTANCE_ID(i);
			UNITY_TRANSFER_INSTANCE_ID(i, o);
			UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);
			o.pos = mul(unity_MatrixVP, mul(unity_ObjectToWorld, half4(i.pos.xyz, 1.0h)));
			half2 offset = _MainTex_TexelSize.xy * _BlurAmount;
			o.uv = UnityStereoTransformScreenSpaceTex(i.uv);
			o.uv1 = half4(o.uv - offset, o.uv + offset);
			return o;
		}

		v2fs verts(appdata i)
		{
			v2fs o = (v2fs)0;
			UNITY_SETUP_INSTANCE_ID(i);
			UNITY_TRANSFER_INSTANCE_ID(i, o);
			UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);
			o.pos = mul(unity_MatrixVP, mul(unity_ObjectToWorld, float4(i.pos.xyz, 1.0h)));
			half2 uv = UnityStereoTransformScreenSpaceTex(i.uv);
			o.uv = float4(uv, uv + _CameraDepthTexture_TexelSize.xy);
			float a = 52.9829189f * dot(float2(0.06711h, 0.005837h), floor(0.5h * uv * _ScreenParams.xy));
			float uv2 = frac(a + 0.112305h) * 2.0h - 1.0h;
			float uv3 = frac(a + 0.1484375h) * 2.0h - 1.0h;
			float uv4 = frac(a + 0.136719h) * 2.0h - 1.0h;
			a *= 6.2831853h;
			o.uv1 = _Radius * _Radius * float4(float2(-0.40825h, 0.40825h) * sqrt(1.0h - uv2 * uv2), float2(0.5h, -0.5h) * sqrt(1.0h - uv3 * uv3));
			o.uv2.xy = _Radius * _Radius * float2(-0.57735h, -0.57735h) * sqrt(1.0h - uv4 * uv4);
			o.uv2.z = a;
			return o;
		}

		float4 frag_ssao(v2fs i) : SV_Target
		{
			UNITY_SETUP_INSTANCE_ID(i);
			UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(i);
			const float3 offset = float3(_CameraDepthTexture_TexelSize.xy, 0.0h);

			float depth = SAMPLE_DEPTH(i.uv.xy);
			float depthu = SAMPLE_DEPTH(i.uv.xy + offset.zy);
			float depthr = SAMPLE_DEPTH(i.uv.xy + offset.xz);

			float3 pu = float3(offset.zy, depthu - depth);
			float3 pr = float3(offset.xz, depthr - depth);

			float3 normal = normalize(cross(pu, pr));

			float s = sin(i.uv2.z);
			float c = cos(i.uv2.z);

			float2 ray = float2(s * -0.9863h - c * 0.1649h, s * 0.1649h - c * 0.9863h) * i.uv1.zw / depth;
			float difference = depth - SAMPLE_DEPTH(saturate(i.uv.xy + sign(dot(ray, normal.xy)) * ray));
			float occlusion = step(0.02h, difference) * (1.0h - smoothstep(0.02h, _Area, difference));
#if !defined(FAST)
			ray = float2(s * -0.38267h - c * 0.92388h, s * 0.92388h - c * 0.38267h) * i.uv1.xy / depth;
			difference = depth - SAMPLE_DEPTH(saturate(i.uv.xy + sign(dot(ray, normal.xy)) * ray));
			occlusion += step(0.02h, difference) * (1.0h - smoothstep(0.02h, _Area, difference));

			ray = float2(s * 0.94953h - c * 0.3137h, s * 0.3137h + c * 0.94953h) * i.uv2.xy / depth;
			difference = depth - SAMPLE_DEPTH(saturate(i.uv.xy + sign(dot(ray, normal.xy)) * ray));
			occlusion += step(0.02h, difference) * (1.0h - smoothstep(0.02h, _Area, difference));

			return 1.0h - _Intensity * occlusion * 0.3333h;
#endif
			return 1.0h - _Intensity * occlusion;
		}

		half4 fragb(v2fb i) : SV_Target
		{
			UNITY_SETUP_INSTANCE_ID(i);
			UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(i);
			half4 col = SAMPLE_TEXTURE2D(_MainTex, sampler_MainTex, i.uv);
			col.r *= 0.5h;
			col.r += SAMPLE_TEXTURE2D(_MainTex, sampler_MainTex, i.uv1.xy).r * 0.125h;
			col.r += SAMPLE_TEXTURE2D(_MainTex, sampler_MainTex, i.uv1.xw).r * 0.125h;
			col.r += SAMPLE_TEXTURE2D(_MainTex, sampler_MainTex, i.uv1.zy).r * 0.125h;
			col.r += SAMPLE_TEXTURE2D(_MainTex, sampler_MainTex, i.uv1.zw).r * 0.125h;
			return col;
		}

		half4 frag(v2f i) : SV_Target
		{
			UNITY_SETUP_INSTANCE_ID(i);
			UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(i);
			half4 col = SAMPLE_TEXTURE2D(_MainTex, sampler_MainTex, i.uv);
			half4 ao = SAMPLE_TEXTURE2D(_BlurTex, sampler_BlurTex, i.uv);
			col.rgb *= ao.r;
			return col;
		}
		ENDHLSL

		ZTest Always Cull Off ZWrite Off
		Fog { Mode off }
        Pass
        {
			Tags{ "RenderPipeline" = "UniversalPipeline" "IgnoreProjector" = "True"}
            HLSLPROGRAM
            #pragma vertex verts
            #pragma fragment frag_ssao
			#pragma shader_feature_local FAST
			ENDHLSL
        }

		ZTest Always Cull Off ZWrite Off
		Fog{ Mode off }
		Pass
		{
			Tags{ "RenderPipeline" = "UniversalPipeline" "IgnoreProjector" = "True"}
			HLSLPROGRAM
			#pragma vertex vertb
			#pragma fragment fragb
			ENDHLSL
		}

		ZTest Always Cull Off ZWrite Off
		Fog{ Mode off }
		Pass
		{
			Tags{ "RenderPipeline" = "UniversalPipeline" "IgnoreProjector" = "True"}
			HLSLPROGRAM
			#pragma vertex vert
			#pragma fragment frag
			ENDHLSL
		}
    }
}

