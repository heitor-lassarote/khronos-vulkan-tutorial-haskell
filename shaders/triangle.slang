struct VSInput {
    float2 inPosition;
    float3 inColor;
};

struct VertexOutput {
    float4 pos : SV_POSITION;
    float3 color;
};

[shader("vertex")]
VertexOutput vertMain(VSInput input) {
    VertexOutput output;
    output.pos = float4(input.inPosition, 0.0, 1.0);
    output.color = input.inColor;
    return output;
}

[shader("fragment")]
float4 fragMain(VertexOutput inVert) : SV_Target {
    return float4(inVert.color, 1.0);
}
