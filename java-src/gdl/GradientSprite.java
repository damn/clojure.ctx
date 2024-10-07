package gdl;

import com.badlogic.gdx.graphics.Color;
import com.badlogic.gdx.graphics.g2d.Batch;
import com.badlogic.gdx.graphics.g2d.Sprite;
import com.badlogic.gdx.graphics.g2d.TextureRegion;

// TODO not used because we just set 1 color for small sprits (maybe for big ones this)
public class GradientSprite extends Sprite {
    public GradientSprite(TextureRegion region) {
        super(region);
    }

    public void setColors(Color c1, Color c2, Color c3, Color c4) {
        setColor(Color.WHITE); // WHY?

        getVertices()[Batch.C1] = c1.toFloatBits();
        getVertices()[Batch.C2] = c2.toFloatBits();
        getVertices()[Batch.C3] = c3.toFloatBits();
        getVertices()[Batch.C4] = c4.toFloatBits();
    }
}
