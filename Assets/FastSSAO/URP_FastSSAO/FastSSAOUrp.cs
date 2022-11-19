namespace UnityEngine.Rendering.Universal
{
    public class FastSSAOUrp : ScriptableRendererFeature
    {
        [System.Serializable]
        public class SSAOSettings
        {
            public RenderPassEvent Event = RenderPassEvent.AfterRenderingTransparents;

            [Range(0, 3)]
            public float Intensity = 2;

            [Range(0, 3)]
            public float BlurAmount = 1;

            [Range(0, 1)]
            public float Radius = 1;

            [Range(0, 4)]
            public float Area = 1;

            public bool FastMode = false;

            public Material BlitMaterial = null;
        }

        public SSAOSettings settings = new SSAOSettings();

        FastSSAOUrpPass fastSSAOUrpPass;

        public override void AddRenderPasses(ScriptableRenderer renderer, ref RenderingData renderingData)
        {
            fastSSAOUrpPass.Setup(renderer.cameraColorTarget);
            renderer.EnqueuePass(fastSSAOUrpPass);
        }

        public override void Create()
        {
            fastSSAOUrpPass = new FastSSAOUrpPass(settings.Event, settings.BlitMaterial, settings.Intensity, settings.BlurAmount, settings.Radius, settings.Area, settings.FastMode, this.name);
        }
    }
}