#let door-sign-one(

  logo-left: none,
  logo-right: none,
  profile-height: none,
  gap-above-profile: 2cm,
  gap-below-profile: 0.6cm,
  font-size: 16pt,
  par-leading: 0.6em,

  person-1-name: none,
  person-1-content: none,
  person-1-profile: none,

  person-2-name: none,
  person-2-content: none,
  person-2-profile: none,

  person-3-name: none,
  person-3-content: none,
  person-3-profile: none,

  person-4-name: none,
  person-4-content: none,
  person-4-profile: none,

  person-5-name: none,
  person-5-content: none,
  person-5-profile: none,

  body

) = {

  // body font.
  set text(font-size, font: "Lato", fill: rgb("#444444"))
  // set par(leading: par-leading)
  // set block(spacing: 1.4em)

  // set document(title: title)

  set page(
    paper: "a4",
    flipped: true,
    margin: (left: 1cm, right: 16cm, top: 3.5cm, bottom: 1.5cm),
    // background: rect(height: 100%, width: 50%, fill: rgb("#FFCBC4")),
    background: place(left,
      rect(height: 100%, width: 50%,
            stroke: (paint: silver, thickness: 1pt, dash: "dashed"))
    ),

    header: grid(
      columns: (1fr, 1fr),
      row-gutter: 0pt,
      align(left, image(logo-left, height: 9mm)),
      align(right, image(logo-right, height: 9mm)),
    )
  )

  // page body
  pad(top: gap-above-profile,
    stack(dir: ttb, spacing: 1.5em,

      grid(
        columns: (30%, 70%),
        row-gutter: gap-below-profile,
        // profile image
        if person-1-profile != none {
          align(left + horizon,
            box(clip: true, stroke: 0pt, radius: profile-height,
              width: profile-height, height: profile-height,
              image(person-1-profile, height: profile-height)
            )
          )
        },
        align(left + horizon,
          stack(
            dir: ttb,
            spacing: 1em,

            // name
            text(size: 1.4em, tracking: 1pt, weight: 600, person-1-name),

            // content
            text(size: 1em, tracking: 0.5pt, person-1-content)
          )
        )
      ),

      grid(
        columns: (30%, 70%),
        row-gutter: 1em,
        // profile image
        if person-2-profile != none {
          align(left + horizon,
            box(clip: true, stroke: 0pt, radius: profile-height,
              width: profile-height, height: profile-height,
              image(person-2-profile, height: profile-height)
            )
          )
        },
        align(left + horizon,
          stack(
            dir: ttb,
            spacing: 1em,

            // name
            text(size: 1.4em, tracking: 1pt, weight: 600, person-2-name),

            // content
            text(size: 1em, tracking: 0.5pt, person-2-content)
          )
        )
      ),

      grid(
      columns: (30%, 70%),
      row-gutter: 1em,
      // profile image
      if person-3-profile != none {
        align(left + horizon,
          box(clip: true, stroke: 0pt, radius: profile-height,
            width: profile-height, height: profile-height,
            image(person-3-profile, height: profile-height)
          )
        )
      },
      align(
        left + horizon,
        stack(
          dir: ttb,
          spacing: 1em,

          // name
          text(size: 1.4em, tracking: 1pt, weight: 600, person-3-name),

          // content
          text(size: 1em, tracking: 0.5pt, person-3-content)
        )
      )
      ),

      grid(
        columns: (30%, 70%),
        row-gutter: 1em,
        // profile image
        if person-4-profile != none {
          align(left + horizon,
            box(clip: true, stroke: 0pt, radius: profile-height,
              width: profile-height, height: profile-height,
              image(person-4-profile, height: profile-height)
            )
          )
        },
        align(
          left + horizon,
          stack(
            dir: ttb,
            spacing: 1em,

            // name
            text(size: 1.4em, tracking: 1pt, weight: 600, person-4-name),

            // content
            text(size: 1em, tracking: 0.5pt, person-4-content)
          )
        )
      ),

      grid(
        columns: (30%, 70%),
        row-gutter: 1em,
        // profile image
        if person-5-profile != none {
          align(left + horizon,
            box(clip: true, stroke: 0pt, radius: profile-height,
              width: profile-height, height: profile-height,
              image(person-5-profile, height: profile-height)
            )
          )
        },
        align(
          left + horizon,
          stack(
            dir: ttb,
            spacing: 1em,

            // name
            text(size: 1.4em, tracking: 1pt, weight: 600, person-5-name),

            // content
            text(size: 1em, tracking: 0.5pt, person-5-content)
          )
        )
      )
   )
  )

  // embellishment
  // place(left, rect(width: 1cm, height: 1cm, fill: rgb("#BDD775"))),

  // body flow
  {
    body
  }
}
