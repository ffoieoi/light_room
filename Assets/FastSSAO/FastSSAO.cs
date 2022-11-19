using UnityEngine;

[RequireComponent(typeof(Camera))]
[ExecuteInEditMode]
public class FastSSAO : MonoBehaviour
{
    [Range(0, 3)]
    public float Intensity = 1;

    [Range(0, 3)]
    public float BlurAmount = 1;

    [Range(0, 1)]
    public float Radius = 1;

    [Range(0, 4)]
    public float Area = 1;

    public bool FastMode;

    public Material material;

    private Camera cam;

    static readonly int blurTexString = Shader.PropertyToID("_BlurTex");
    static readonly int instensityString = Shader.PropertyToID("_Intensity");
    static readonly int blurAmountString = Shader.PropertyToID("_BlurAmount");
    static readonly int radiusString = Shader.PropertyToID("_Radius");
    static readonly int areaString = Shader.PropertyToID("_Area");

    static readonly string fastKeyword = "FAST";

    private void Awake()
    {
        cam = GetComponent<Camera>();
        cam.depthTextureMode = DepthTextureMode.Depth;
    }

    private void OnRenderImage(RenderTexture source, RenderTexture destination)
    {
        material.SetFloat(instensityString, Intensity);
        material.SetFloat(blurAmountString, BlurAmount);
        material.SetFloat(radiusString, Radius * 0.5f);
        material.SetFloat(areaString, Area + 0.1f);
        if (FastMode)
            material.EnableKeyword(fastKeyword);
        else
            material.DisableKeyword(fastKeyword);

        var blurTex = RenderTexture.GetTemporary(Screen.width, Screen.height, 0, source.format);
        var temp = RenderTexture.GetTemporary(Screen.width / 2, Screen.height / 2, 0, source.format);
        var temp1 = RenderTexture.GetTemporary(Screen.width / 4, Screen.height / 4, 0, source.format);

        Graphics.Blit(source, blurTex, material, 0);
        Graphics.Blit(blurTex, temp, material, 1);
        Graphics.Blit(temp, temp1, material, 1);
        RenderTexture.ReleaseTemporary(temp);
        material.SetTexture(blurTexString, temp1);
        RenderTexture.ReleaseTemporary(blurTex);
        RenderTexture.ReleaseTemporary(temp1);

        Graphics.Blit(source, destination, material, 2);
    }
}
