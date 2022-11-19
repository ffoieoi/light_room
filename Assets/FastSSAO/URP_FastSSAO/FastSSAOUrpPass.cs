namespace UnityEngine.Rendering.Universal
{
    public class FastSSAOUrpPass : ScriptableRenderPass
    {
        private RenderTargetIdentifier source;
        private RenderTargetIdentifier blurTemp = new RenderTargetIdentifier(blurTempString);
        private RenderTargetIdentifier blurTemp1 = new RenderTargetIdentifier(blurTemp1String);
        private RenderTargetIdentifier blurTex = new RenderTargetIdentifier(blurTexString);
        private RenderTargetIdentifier tempCopy = new RenderTargetIdentifier(tempCopyString);

        static readonly int blurTexString = Shader.PropertyToID("_BlurTex");
        static readonly int blurTempString = Shader.PropertyToID("_BlurTemp");
        static readonly int blurTemp1String = Shader.PropertyToID("_BlurTemp1");
        static readonly int tempCopyString = Shader.PropertyToID("_TempCopy");
        static readonly int instensityString = Shader.PropertyToID("_Intensity");
        static readonly int blurAmountString = Shader.PropertyToID("_BlurAmount");
        static readonly int radiusString = Shader.PropertyToID("_Radius");
        static readonly int areaString = Shader.PropertyToID("_Area");
        static readonly string fastKeyword = "FAST";

        private readonly float intensity;
        private readonly float blurAmount;
        private readonly float radius;
        private readonly float area;
        private readonly bool fastMode;
        private readonly string tag;


        public Material material;   

        public FastSSAOUrpPass(RenderPassEvent renderPassEvent, Material material,
            float intensity, float blurAmount, float radius, float area, bool fastMode, string tag)
        {
            this.renderPassEvent = renderPassEvent;
            this.intensity = intensity;
            this.blurAmount = blurAmount;
            this.radius = radius;
            this.area = area;
            this.fastMode = fastMode;
            this.tag = tag;
            this.material = material;
        }

        public void Setup(RenderTargetIdentifier source)
        {
            this.source = source;
        }

        public override void Execute(ScriptableRenderContext context, ref RenderingData renderingData)
        {
            CommandBuffer cmd = CommandBufferPool.Get(tag);
            RenderTextureDescriptor opaqueDesc = renderingData.cameraData.cameraTargetDescriptor;
            opaqueDesc.depthBufferBits = 0;

            material.SetFloat(instensityString, intensity);
            material.SetFloat(blurAmountString, blurAmount);
            material.SetFloat(radiusString, radius * 0.5f);
            material.SetFloat(areaString, area + 0.1f);
            if (fastMode)
                material.EnableKeyword(fastKeyword);
            else
                material.DisableKeyword(fastKeyword);

            cmd.GetTemporaryRT(tempCopyString, opaqueDesc, FilterMode.Bilinear);
            cmd.CopyTexture(source, tempCopy);
            cmd.GetTemporaryRT(blurTexString, Screen.width/4 , Screen.height/4 , 0, FilterMode.Bilinear);
            cmd.GetTemporaryRT(blurTempString, Screen.width / 2, Screen.height / 2, 0, FilterMode.Bilinear);
            cmd.GetTemporaryRT(blurTemp1String, Screen.width, Screen.height, 0, FilterMode.Bilinear);
            cmd.Blit(tempCopy, blurTemp1, material, 0);
            cmd.Blit(blurTemp1, blurTemp, material, 1);
            cmd.Blit(blurTemp, blurTex, material, 1);

            cmd.Blit(tempCopy, source, material, 2);

            context.ExecuteCommandBuffer(cmd);
            CommandBufferPool.Release(cmd);
        }

        public override void FrameCleanup(CommandBuffer cmd)
        {
            cmd.ReleaseTemporaryRT(tempCopyString);
            cmd.ReleaseTemporaryRT(blurTexString);
            cmd.ReleaseTemporaryRT(blurTempString);
            cmd.ReleaseTemporaryRT(blurTemp1String);
        }
    }
}
