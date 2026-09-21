window.RevealQdraw = function () {
  return {
    id: "RevealQdraw",
    init: function (deck) {
      // Create and inject HTML elements for the drawing canvas and controls
      const controlsWrapper = document.createElement('div');
      controlsWrapper.id = 'controlsWrapper';
      controlsWrapper.innerHTML = `
        <button id="toggleControls" title="Toggle Drawing Controls" style="font-size: 1.9em; color: red;">
          <i class="fas fa-pen-nib"></i>
        </button>
        <div id="controls">
          <label style="font-weight: bold; color: #8f246b;" id="penTool" title="Pen Tool"><i class="fas fa-marker"></i></label>
          <input type="color" id="penColor" value="#000000" style="display: none;" />
          <label for="penColor" id="penColorLabel" title="Pen Color">
            <i class="fas fa-eye-dropper"></i>
          </label>
          <input type="range" id="penSize" min="1" max="20" value="4" />
          <label style="font-weight: bold; color: #408000;" id="undo" title="Undo drawing"><i class="fas fa-undo"></i></label>
          <label style="font-weight: bold; color: red;" id="eraserTool" title="Eraser Tool"><i class="fas fa-eraser"></i></label>
          <input type="range" id="eraserSize" min="20" max="250" value="60" />
          <label style="font-weight: bold; color: red;" id="clear" type="button" title="Delete All drawing"><i class="fas fa-trash"></i></label>
          <label style="font-weight: bold; color: green;" for="bgColor" id="bgColorLabel" title="Canvas Background">
            <i class="fas fa-fill-drip"></i>
          </label>
          <input type="color" id="bgColor" style="display: none;" />
          <label style="font-weight: bold; color: red;" id="resetBg" title="Reset background color"><i class="fas fa-fill-drip"></i></label>
          <label style="font-weight: bold; color: #408000;" id="prevPage" title="Previous page"><i class="fas fa-chevron-left"></i></label>
          <span id="pageLabel" title="Current page">1/1</span>
          <label style="font-weight: bold; color: #408000;" id="nextPage" title="Next page"><i class="fas fa-chevron-right"></i></label>
          <label style="font-weight: bold; color: #8f246b;" id="addPage" title="Add new page"><i class="fas fa-plus"></i></label>
          <label style="font-weight: bold; color: green;" id="downloadCanvas" title="Download Drawing" class="icon-button">
            <i class="fas fa-download"></i>
          </label>
          <label style="font-weight: bold; color: #3366cc;" id="about" title="About this tool">
            <i class="fas fa-circle-info"></i>
          </label>
        </div>
        <div id="aboutPopover">
          Developed by Abdullah Al Mahmud<br>
          <a href="https://www.thinkermahmud.com/qdraw" target="_blank" rel="noopener">Learn more</a>
        </div>
      `;
      document.body.appendChild(controlsWrapper);

      // Sits on the opposite edge from the toolbar so it's reachable
      // whichever side the toolbar currently sits on (helpful on wide screens).
      const moveControls = document.createElement('button');
      moveControls.id = 'moveControls';
      moveControls.title = 'Move controls to the other side';
      moveControls.innerHTML = '<i class="fas fa-right-left"></i>';
      document.body.appendChild(moveControls);

      const eraserCursor = document.createElement('div');
      eraserCursor.id = 'eraserCursor';
      document.body.appendChild(eraserCursor);

      const canvas = document.createElement('canvas');
      canvas.id = 'drawingCanvas';
      document.body.appendChild(canvas);

      // Get the injected elements
      const ctx = canvas.getContext('2d');
      const bgColorPicker = document.getElementById('bgColor');
      const resetBgBtn = document.getElementById('resetBg');
      const penColorInput = document.getElementById('penColor');
      const penIcon = document.querySelector('#penColorLabel i');
      const penSize = document.getElementById('penSize');
      const eraserSize = document.getElementById('eraserSize');
      const penTool = document.getElementById('penTool');
      const undo = document.getElementById('undo');
      const eraserTool = document.getElementById('eraserTool');
      const clearBtn = document.getElementById('clear');
      const prevPageBtn = document.getElementById('prevPage');
      const nextPageBtn = document.getElementById('nextPage');
      const addPageBtn = document.getElementById('addPage');
      const pageLabel = document.getElementById('pageLabel');
      const toggleControls = document.getElementById('toggleControls');
      const controls = document.getElementById('controls');
      const downloadButton = document.getElementById('downloadCanvas');
      const about = document.getElementById('about');
      const aboutPopover = document.getElementById('aboutPopover');

      // Set initial color
      penIcon.style.color = penColorInput.value;

      // Variables for drawing state
      let drawing = false, erasing = false;
      let controlsEnabled = false;
      let lastX = 0, lastY = 0;
      const defaultBgColor = "";

      // Each page keeps its own saved snapshot (as a data URL, restored via drawImage
      // so it survives canvas resizes) and its own undo stack. `history` always points
      // at the current page's stack.
      let currentPage = 0;
      const pages = [null];
      const pageHistories = [[]];
      let history = pageHistories[currentPage];

      function updatePageLabel() {
        pageLabel.textContent = (currentPage + 1) + '/' + pages.length;
      }

      function loadPage(index) {
        ctx.clearRect(0, 0, canvas.width, canvas.height);
        const src = pages[index];
        if (src) {
          const img = new Image();
          img.onload = () => ctx.drawImage(img, 0, 0, canvas.width, canvas.height);
          img.src = src;
        }
      }

      function goToPage(index) {
        if (index < 0 || index >= pages.length || index === currentPage) return;
        pages[currentPage] = canvas.toDataURL();
        pageHistories[currentPage] = history;
        currentPage = index;
        history = pageHistories[currentPage];
        loadPage(currentPage);
        updatePageLabel();
      }

      // --- Functions ---
      function resize() {
        canvas.width = window.innerWidth;
        canvas.height = window.innerHeight;
      }

      function pos(e) {
        if (e.touches) e = e.touches[0];
        // Map viewport coordinates to canvas pixel coordinates via the canvas's
        // actual rendered rect, rather than assuming clientX/Y == canvas pixels.
        // On mobile, the canvas's CSS box (100vw/100vh) and its backing store
        // (sized from window.innerWidth/innerHeight) can drift apart (e.g. when
        // the browser's address bar shows/hides), which otherwise makes strokes
        // land away from the pointer.
        const rect = canvas.getBoundingClientRect();
        const scaleX = canvas.width / rect.width;
        const scaleY = canvas.height / rect.height;
        return { x: (e.clientX - rect.left) * scaleX, y: (e.clientY - rect.top) * scaleY };
      }

      function updateEraser(x, y) {
        const size = Number(eraserSize.value);
        eraserCursor.style.width = size + 'px';
        eraserCursor.style.height = size + 'px';
        eraserCursor.style.left = x - size / 2 + 'px';
        eraserCursor.style.top = y - size / 2 + 'px';
      }

      function start(e) {
        if (!controlsEnabled) return;
        drawing = true;
        // Store a snapshot before starting to draw for undo functionality
        history.push(ctx.getImageData(0, 0, canvas.width, canvas.height));
        const p = pos(e);
        lastX = p.x;
        lastY = p.y;
        draw(e);
      }

      function end() {
        drawing = false;
        ctx.globalCompositeOperation = 'source-over';
      }

      function draw(e) {
        if (!drawing) return;
        const p = pos(e);
        const size = erasing ? Number(eraserSize.value) : Number(penSize.value);
        ctx.lineWidth = size;
        ctx.lineCap = 'round';
        ctx.globalCompositeOperation = erasing ? 'destination-out' : 'source-over';
        ctx.strokeStyle = penColorInput.value;
        if (erasing) updateEraser(p.x, p.y);
        ctx.beginPath();
        ctx.moveTo(lastX, lastY);
        ctx.lineTo(p.x, p.y);
        ctx.stroke();
        lastX = p.x;
        lastY = p.y;
      }

      // --- Event Listeners ---
      window.addEventListener('resize', resize);
      resize(); // Initial resize

      canvas.addEventListener('pointerdown', start);
      canvas.addEventListener('pointermove', draw);
      canvas.addEventListener('pointerup', end);
      canvas.addEventListener('pointercancel', end);

      canvas.addEventListener('pointerdown', function(e) {
        this.setPointerCapture(e.pointerId);
      });
      canvas.addEventListener('pointerup', function(e) {
        this.releasePointerCapture(e.pointerId);
      });

      penColorInput.addEventListener('input', () => {
        penIcon.style.color = penColorInput.value;
      });

      penTool.onclick = () => {
        erasing = false;
        eraserCursor.style.display = 'none';
        canvas.style.pointerEvents = 'auto';
      };

      eraserTool.onclick = () => {
        erasing = true;
        eraserCursor.style.display = 'block';
        canvas.style.pointerEvents = 'auto';
      };

      undo.onclick = () => {
        if (history.length) {
          ctx.putImageData(history.pop(), 0, 0);
        }
      };

      clearBtn.addEventListener('click', () => {
        ctx.clearRect(0, 0, canvas.width, canvas.height);
        history.length = 0;
      });

      bgColorPicker.addEventListener('input', () => {
        canvas.style.backgroundColor = bgColorPicker.value;
      });

      resetBgBtn.addEventListener('click', () => {
        canvas.style.backgroundColor = defaultBgColor;
        bgColorPicker.value = "#ffffff";
      });

      prevPageBtn.addEventListener('click', () => {
        goToPage(currentPage - 1);
      });

      nextPageBtn.addEventListener('click', () => {
        goToPage(currentPage + 1);
      });

      addPageBtn.addEventListener('click', () => {
        pages[currentPage] = canvas.toDataURL();
        pageHistories[currentPage] = history;
        pages.push(null);
        pageHistories.push([]);
        currentPage = pages.length - 1;
        history = pageHistories[currentPage];
        ctx.clearRect(0, 0, canvas.width, canvas.height);
        updatePageLabel();
      });

      toggleControls.onclick = () => {
        const isVisible = controls.style.display === 'flex';
        controls.style.display = isVisible ? 'none' : 'flex';
        controlsEnabled = !isVisible;
        canvas.style.pointerEvents = controlsEnabled ? 'auto' : 'none';
        if (erasing && controlsEnabled) {
          eraserCursor.style.display = 'block';
        } else {
          eraserCursor.style.display = 'none';
        }
        aboutPopover.classList.remove('show');
      };

      about.onclick = (e) => {
        e.stopPropagation();
        aboutPopover.classList.toggle('show');
      };

      document.addEventListener('click', (e) => {
        if (aboutPopover.classList.contains('show') && !aboutPopover.contains(e.target) && e.target !== about) {
          aboutPopover.classList.remove('show');
        }
      });

      moveControls.onclick = () => {
        controlsWrapper.classList.toggle('right');
        moveControls.classList.toggle('left');
      };

      downloadButton.addEventListener('click', function () {
        if (!canvas) return;
        const link = document.createElement('a');
        link.href = canvas.toDataURL('image/png');
        link.download = 'drawing.png';
        link.click();
      });
    },
  };
};
