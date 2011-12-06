function drawText (ctx, font, size, x, y, str, rot, adj)
{
    ctx.save();
    if(font && font != "")
       ctx.font = font;

    ctx.translate(x, y);
    if(rot != 0.0) 
        ctx.rotate(-1 *Math.PI/180 * rot);

    ctx.strokeText(str, 0, 0);
    ctx.restore();
}
